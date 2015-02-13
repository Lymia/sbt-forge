package moe.lymia.sbt

import sbt._
import Keys._

import java.net.URL
import java.io._

import play.api.libs.json._

import LWJGLSupport._
import forge._
import forge.asm._

import language._

object ForgePlugin extends Plugin {
  object forge { 
    // User setting keys
    val mcVersion   = SettingKey[String]("forge-minecraft-version")
    val version     = SettingKey[String]("forge-version")
    val fullVersion = SettingKey[String]("forge-full-version")

    val cacheDir   = SettingKey[File]("forge-cache-dir",
      "Directory used to store files used by sbt-forge")
    val forgeDir   = SettingKey[File]("forge-forge-dir",
      "Directory used to store files specific to a forge version")
    val dlCacheDir = SettingKey[File]("forge-minecraft-dir",
      "Directory used to store files specific to a Minecraft version")

    val cleanDlCache = SettingKey[Boolean]("forge-clean-download-cache",
      "If set, sbt clean will erase the Download cache directory and force a redownload.")

    // URL locations
    val clientDownloadUrl    = TaskKey[String]("forge-client-download-url",
      "Download URL for the Minecraft client binary")
    val serverDownloadUrl    = TaskKey[String]("forge-server-download-url",
      "Download URL for the Minecraft server binary")
    val universalDownloadUrl = TaskKey[String]("forge-universal-download-url",
      "Download URL for the Forge binary")
    val userdevDownloadUrl   = TaskKey[String]("forge-userdev-download-url",
      "Download URL for the Userdev archive")

    // Download needed .jars
    val clientJar      = TaskKey[File]("forge-client-jar",
      "Location of the client jar. By default, downloads from Minecraft's CDN.")
    val serverJar      = TaskKey[File]("forge-server-jar",
      "Location of the server jar. By default, downloads from Minecraft's CDN.")
    val universalJar   = TaskKey[File]("forge-universal-jar", 
      "Location of the Forge universal jar. By default, downloads from Forge's website.")
    val userdevArchive = TaskKey[File]("forge-userdev-archive", 
      "Location of the Forge userdev archive. By default, downloads from Forge's website.")

    // Extract files needed by later steps.
    val dependenciesJson = TaskKey[File]("forge-dependencies-json",
      "Location of the .json file declaring Minecraft's dependencies. "+
      "By default, extracts dev.json from the userdev archive.")
    val binpatches       = TaskKey[File]("forge-binpatches",
      "Binary patches to patch server and client jars with. "+
      "By default, extracts Forge's patches from the universal binary")
    val mergeConfig      = TaskKey[File]("forge-merge-config",
      "Configuration for merging the client and server jars. "+
      "By default, extracts Forge's merge configuration from the userdev archive")
    val srgFile           = TaskKey[File]("forge-srg-file",
      "The .srg file used for notch->SRG deobf. "+
      "By default, extracts packaged.srg from the userdev archive.")
    val exceptorJson      = TaskKey[File]("forge-exceptor-json",
      "The .json file used to restore inner/outer class attributes to classes. "+
      "By default, extracts exceptor.json from the userdev archive.")
    val excFile           = TaskKey[File]("forge-exc-file",
      "The .exc file used to restore exception data, add __OBFID fields, and constructor parameter names to classes. "+
      "By default, extracts packaged.json from the userdev archive.")

    // Load forge's dependency .json file
    val excludedOrganizations    = SettingKey[Set[String]]("forge-excluded-organizations",
      "Organizations excluded from dependency autoloading.")
    val loadDependenciesFromJson = TaskKey[Seq[ModuleID]]("forge-load-dependencies",
      "Loads Forge's dependencies from dev.json")

    // Patch and merge client .jars
    val patchServerJar = TaskKey[File]("forge-patch-server-jar",
      "Applies Forge's patches to the Minecraft server binary")
    val patchClientJar = TaskKey[File]("forge-patch-client-jar",
      "Applies Forge's patches to the Minecraft client binary")
    val mergeJars      = TaskKey[File]("forge-merge-jars",
      "Merges the Minecraft client and server binaries.")

    // Deobf merged .jar to SRG names
    val mergedForgeBinary = TaskKey[File]("forge-merged-forge-binary",
      "Deobfs and merges the Minecraft binary and the Minecraft Forge binary to SRG names.")
  }

  object forgeHelpers {
    def defaultDownloadUrl(section: String) =
      forge.fullVersion map { ver =>
        "http://files.minecraftforge.net/maven/net/minecraftforge/forge/" +
          ver + "/forge-" + ver + "-" + section + ".jar"
      }

    def copyUrl(target: File, source: String, log: Logger, verb: String) = {
      if(!target.exists) {
        if(!target.getParentFile.exists)
          if(!target.getParentFile.mkdirs())
            sys.error("Failed to create parent directory of "+target)
        log.info(verb+" "+source+" to "+target+"...")
        new URL(source) #> target !!;
        log.info("Done "+verb.toLowerCase+" "+source+".")
      } else {
        log.info(target+" already exists, skipping.")
      }
      target
    }
    def copyUrlTask[T](task: TaskKey[File], versionTask: SettingKey[String],
                       urlSource: TaskKey[T], outputName: String => String, 
                       targetDir: SettingKey[File],
                       urlFilter: T => String = (x: Any) => x.toString,
                       verb: String = "Copying") =
      task := copyUrl(targetDir.value / outputName(versionTask.value), urlFilter(urlSource.value), streams.value.log, verb)

    def jarFileUrl(jar: File, file: String) =
      "jar:"+jar.toURI.toURL+"!/"+file

    // TODO: Add dependency checking
    def cachedFile[T](outFile: File)(task: File => T) = {
      if(!outFile.exists) task(outFile)
      outFile
    }
    def patchJarTask(task: TaskKey[File],
                     input: TaskKey[File], outputName: String, patchSection: String) =
      task := cachedFile(forge.forgeDir.value / outputName) { outFile =>
        val patchSet = BinPatch.readPatchSet(forge.binpatches.value, patchSection)
        BinPatch.patchJar(input.value, outFile, patchSet, streams.value.log)
      }

    def extractTask(task: TaskKey[File], urlSource: TaskKey[File], sourceName: String, outputName: String, targetDir: SettingKey[File]) =
      copyUrlTask(task, forge.fullVersion, urlSource, _ => outputName, targetDir,
                  urlFilter = (x: File) => jarFileUrl(x, sourceName),
                  verb = "Extracting")
    def downloadTask(task: TaskKey[File], versionKey: SettingKey[String], urlSource: TaskKey[String],
                     outputName: String, ext: String) =
      copyUrlTask(task, versionKey, urlSource, version => outputName+"-"+version+ext, forge.dlCacheDir, 
                  verb = "Downloading")
  }
  import forgeHelpers._

  lazy val forgeSettingsBase: Seq[Setting[_]] = lwjglSettings ++ Seq(
    forge.fullVersion  := forge.mcVersion.value + "-" + forge.version.value,

    forge.cacheDir   := target.value / "sbt-forge-cache",
    forge.forgeDir   := forge.cacheDir.value / ("forge-"+forge.fullVersion.value),
    forge.dlCacheDir := target.value / "sbt-forge-dlcache",

    forge.cleanDlCache := false,

    // Download needed files
    forge.clientDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/"+ver+".jar"),
    forge.serverDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/minecraft_server."+ver+".jar"),
    forge.universalDownloadUrl <<= defaultDownloadUrl("universal"),
    forge.userdevDownloadUrl   <<= defaultDownloadUrl("userdev"),

    downloadTask(forge.clientJar     , forge.mcVersion  , forge.clientDownloadUrl   , "minecraft_client", ".jar"),
    downloadTask(forge.serverJar     , forge.mcVersion  , forge.serverDownloadUrl   , "minecraft_server", ".jar"),
    downloadTask(forge.universalJar  , forge.fullVersion, forge.universalDownloadUrl, "forge_universal" , ".jar"),
    downloadTask(forge.userdevArchive, forge.fullVersion, forge.userdevDownloadUrl  , "forge_userdev"   , ".jar"),

    // Extract files used in later processes
    extractTask(forge.binpatches      , forge.universalJar  , "binpatches.pack.lzma", "binpatches.pack.lzma", forge.forgeDir),
    extractTask(forge.mergeConfig     , forge.userdevArchive, "conf/mcp_merge.cfg"  , "mcp_merge.cfg"       , forge.forgeDir),
    extractTask(forge.srgFile         , forge.userdevArchive, "conf/packaged.srg"   , "packaged.srg"        , forge.forgeDir),
    extractTask(forge.exceptorJson    , forge.userdevArchive, "conf/exceptor.json"  , "exceptor.json"       , forge.forgeDir),
    extractTask(forge.excFile         , forge.userdevArchive, "conf/packaged.exc"   , "packaged.exc"        , forge.forgeDir),
    extractTask(forge.dependenciesJson, forge.userdevArchive, "dev.json"            , "dev.json"            , forge.forgeDir),

    // Set up dependency resolution
    resolvers += "forge" at "http://files.minecraftforge.net/maven",
    resolvers += "minecraft" at "https://libraries.minecraft.net/",
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += Resolver.sonatypeRepo("snapshots"),

    forge.excludedOrganizations := Set("org.scala-lang", "org.scala-lang.plugins", "org.lwjgl.lwjgl"),
    forge.loadDependenciesFromJson :=
      (Json.parse(IO.read(forge.dependenciesJson.value)) \ "libraries").as[Seq[JsObject]].map { elem =>
        val Array(org, project, version) = (elem \ "name").as[String].split(":")
        org % project % version
      }.filter(x => !forge.excludedOrganizations.value.contains(x.organization)),
    // TODO: Add references to the repo versions of scala libraries.
    allDependencies <++= forge.loadDependenciesFromJson,

    // Process jars to merged SRG Forge binary
    patchJarTask(forge.patchClientJar, forge.clientJar     , "minecraft_client_patched.jar", "client"),
    patchJarTask(forge.patchServerJar, forge.serverJar     , "minecraft_server_patched.jar", "server"),
    forge.mergeJars := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / "minecraft_merged.jar") { outFile =>
        log.info("Merging client and server binaries to "+outFile)
        writeJarFile(Merger.merge(
          loadJarFile(new FileInputStream(forge.patchClientJar.value)),
          loadJarFile(new FileInputStream(forge.patchServerJar.value)),
          IO.readLines(forge.mergeConfig.value), log
        ), new FileOutputStream(outFile))
      }
    },
    forge.mergedForgeBinary := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / "forgeBin_srg.jar") { outFile => 
        val classpath = (fullClasspath in Compile).value.map(_.data)
        val minecraftNotch = loadJarFile(new FileInputStream(forge.mergeJars.value))
        val forgeNotch = loadJarFile(new FileInputStream(forge.universalJar.value))
        val map = mapping.readMappingFromSrg(new FileInputStream(forge.srgFile.value))

        log.info("Deobfing merged Minecraft binary to SRG names...")
        val minecraftSrg = Renamer.applyMapping(minecraftNotch, classpath :+ forge.universalJar.value, map, log)
        log.info("Restoring class attributes...")
        Exceptor.applyExceptorJson(minecraftSrg, IO.read(forge.exceptorJson.value), log)
        Exceptor.applyExcFile(minecraftSrg, new FileInputStream(forge.excFile.value), log)

        log.info("Deobfing Forge binary to SRG names...")
        val forgeSrg = Renamer.applyMapping(forgeNotch, classpath :+ forge.mergeJars.value, map, log,
                                            fixInnerClasses = true)

        log.info("Merging Forge binary and Minecraft binary to "+outFile)
        writeJarFile(Merger.addForgeClasses(minecraftSrg, forgeSrg, log), new FileOutputStream(outFile))
      }
    },

    clean := {
      // Clean the resolution cache before cleaning up files.
      //
      // sbt-forge will still try to download userdev to find its dependencies... because ivyModules reads
      // allDependencies, but, it won't leave behind garbage, at least.

      val method = IvyActions.getClass.getDeclaredMethod("cleanCachedResolutionCache", classOf[IvySbt#Module], classOf[Logger])
      method.setAccessible(true)
      method.invoke(IvyActions, ivyModule.value, streams.value.log)
      Defaults.doClean(cleanFiles.value, cleanKeepFiles.value)
    },
    cleanKeepFiles ++= (if(forge.cleanDlCache.value) Seq() else Seq(forge.dlCacheDir.value))
  )
  lazy val forgeSettings_1_7_10 = forgeSettingsBase ++ Seq(
    forge.mcVersion := "1.7.10",
    forge.version   := "10.13.2.1291",
    scalaVersion    := "2.11.1"
  )
  lazy val forgeSettings_1_8 = forgeSettingsBase ++ Seq(
    forge.mcVersion := "1.8",
    forge.version   := "11.14.0.1299",
    scalaVersion    := "2.11.1"
  )
}
