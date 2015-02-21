package moe.lymia.sbt

import sbt._
import Keys._

import java.net.URL
import java.io._

import play.api.libs.json._

import LWJGLSupport._
import forge._
import forge.asm._
import forge.mapping._

import language._

// TODO: Add support for 'forge-internal' mapping to pull mappings from userdev archive.
// TODO: Split this into serveral files?
object ForgePlugin extends Plugin {
  object forge { 
    // User setting keys
    val mcVersion    = SettingKey[String]("forge-minecraft-version")
    val version      = SettingKey[String]("forge-version")
    val fullVersion  = SettingKey[String]("forge-full-version")

    val mappings     = SettingKey[String]("forge-mappings",
      "Mapping from SRG names to MCP names to use")

    val isRenamed    = SettingKey[Boolean]("forge-is-renamed",
      "Used by merger. True if cpw.mods.fml.relauncher has been moved to net.minecraftforge.fml.relauncher")

    val cacheRoot    = SettingKey[File]("forge-cache-root")

    val buildDir     = SettingKey[File]("forge-build-dir",
      "Directory used to store temporary build files used by sbt-forge")
    val forgeDir     = SettingKey[File]("forge-forge-dir",
      "Directory used to store files specific to a forge version")

    val ltCacheDir   = SettingKey[File]("forge-lt-dir",
      "Directory used to store persistant files generated by sbt-forge")
    val dlCacheDir   = SettingKey[File]("forge-download-dir",
      "Directory used to store files downloaded by sbt-forge")
    val assetsDir    = SettingKey[File]("forge-asseets-dir",
      "Directory used to store Minecraft assets by sbt-forge ")

    val runDir       = SettingKey[File]("forge-run-dir",
      "Directory used at runtime by Minecraft")

    val cleanLtCache = SettingKey[Boolean]("forge-clean-download-cache",
      "If set, sbt clean will erase the long term cache directory, containing downloads and access tokens")
    val cleanRunDir  = SettingKey[Boolean]("forge-clean-run-dir",
      "If set, sbt clean will erase the runtime directory, containing Minecraft configuration and saves")

    // Debug keys
    val debugDir          = SettingKey[File]("forge-debug-dir",
      "Directory used to store debug files dumped by sbt-forge")
    val debugDumpMappings = SettingKey[Boolean]("forge-debug-dump-mappings",
      "If set, sbt-forge will dump the temporary mappings used to actually remap .jar files")

    // URL locations
    val clientDownloadUrl    = TaskKey[String]("forge-client-download-url",
      "Download URL for the Minecraft client binary")
    val serverDownloadUrl    = TaskKey[String]("forge-server-download-url",
      "Download URL for the Minecraft server binary")
    val universalDownloadUrl = TaskKey[String]("forge-universal-download-url",
      "Download URL for the Forge binary")
    val userdevDownloadUrl   = TaskKey[String]("forge-userdev-download-url",
      "Download URL for the Userdev archive")
    val mappingDownloadUrl   = TaskKey[String]("forge-mapping-url",
      "URL to download MCP mappings from")

    // Download needed files
    val clientJar      = TaskKey[File]("forge-client-jar",
      "Location of the client jar. By default, downloads from Minecraft's CDN")
    val serverJar      = TaskKey[File]("forge-server-jar",
      "Location of the server jar. By default, downloads from Minecraft's CDN")
    val universalJar   = TaskKey[File]("forge-universal-jar", 
      "Location of the Forge universal jar. By default, downloads from Forge's website")
    val userdevArchive = TaskKey[File]("forge-userdev-archive", 
      "Location of the Forge userdev archive. By default, downloads from Forge's website")
    val mappingArchive = TaskKey[File]("forge-mapping-archive",
      "Location of the MCP mapping archive. By default, downloads from Forge's website")
    val assetsIndex    = TaskKey[File]("forge-assets-index",
      "Location of Minecraft's asset index file. By default, downloads from Minecraft's CDN")

    // Extract files needed by later steps.
    val dependenciesJson = TaskKey[File]("forge-dependencies-json",
      "Location of the .json file declaring Minecraft's dependencies. "+
      "By default, extracts dev.json from the userdev archive")
    val binpatches       = TaskKey[File]("forge-binpatches",
      "Binary patches to patch Minecraft classes with.")
    val mergeConfig      = TaskKey[File]("forge-merge-config",
      "Configuration for merging the client and server jars. "+
      "By default, extracts Forge's merge configuration from the userdev archive")
    val srgFile           = TaskKey[File]("forge-srg-file",
      "The .srg file used for notch->SRG deobf. "+
      "By default, extracts packaged.srg from the userdev archive")
    val exceptorJson      = TaskKey[File]("forge-exceptor-json",
      "The .json file used to restore inner/outer class attributes to classes. "+
      "By default, extracts exceptor.json from the userdev archive")
    val excFile           = TaskKey[File]("forge-exc-file",
      "The .exc file used to restore exception data, add __OBFID fields, and constructor parameter names to classes. "+
      "By default, extracts packaged.json from the userdev archive")

    // Load forge's dependency .json file
    val excludedOrganizations    = SettingKey[Set[String]]("forge-excluded-organizations",
      "Organizations excluded from dependency autoloading")
    val loadDependenciesFromJson = TaskKey[Seq[ModuleID]]("forge-load-dependencies",
      "Loads Forge's dependencies from dev.json")

    // Patch and merge client .jars
    val patchServerJar = TaskKey[File]("forge-patch-server-jar",
      "Applies Forge's patches to the Minecraft server binary")
    val patchClientJar = TaskKey[File]("forge-patch-client-jar",
      "Applies Forge's patches to the Minecraft client binary")
    val fgMergeJars    = TaskKey[File]("forge-fg-merge-jars",
      "Merges the Minecraft client and server binaries, using ForgeGradle's merger code")
    val mergedJar      = TaskKey[File]("forge-merge-jars",
      "Merged and patched Minecraft binary")

    // Deobf merged .jar to SRG names
    val accessTransformers = TaskKey[Seq[File]]("forge-access-transformers",
      "List of access transformers to be applied to the Forge binary")
    val mergedForgeBinary  = TaskKey[File]("forge-merged-forge-binary",
      "Deobfs and merges the Minecraft binary and the Minecraft Forge binary to SRG names")

    // Remap .jar files from SRG names to MCP names
    val mappingCache = TaskKey[File]("forge-mapping-cache",
      "Cached mapping generated from .csv files")
    val forgeBinary  = TaskKey[File]("forge-binary",
      "Forge binary remapped to MCP names")

    // Run Minecraft
    val runOptions     = TaskKey[ForkOptions]("forge-run-options",
      "Fork options for running Minecraft")
    val prepareRunDir  = TaskKey[Unit]("forge-prepare-run-dir",
      "Prepares the run directory for Minecraft")
    val downloadAssets = TaskKey[Unit]("forge-download-assets",
      "Downloads assets for the current Minecraft version")
    val runClient      = InputKey[Unit]("run-client", "Runs the Minecraft client.")
    val runServer      = InputKey[Unit]("run-server", "Runs the Minecraft server")

    // Clean all
    val cleanCache    = TaskKey[Unit]("clean-cache",
      "Cleans sbt-forge's long term cache")
  }

  object forgeHelpers {
    def defaultDownloadUrl(section: String) =
      forge.fullVersion map { ver =>
        "http://files.minecraftforge.net/maven/net/minecraftforge/forge/" +
          ver + "/forge-" + ver + "-" + section + ".jar"
      }

    def copyUrl(target: File, source: String, log: Logger, verb: String) = {
      if(!target.exists) {
        forgeHelpers synchronized {
          if(!target.getParentFile.exists)
            if(!target.getParentFile.mkdirs())
              sys.error("Failed to create parent directory of "+target)
        }
        log.info(verb+" "+source+" to "+target+"...")
        new URL(source) #> target !!
      }
      target
    }

    def jarFileUrl(jar: File, file: String) =
      "jar:"+jar.toURI.toURL+"!/"+file

    // TODO: Add dependency checking
    def cachedFile[T](outFile: File)(task: File => T) = {
      if(!outFile.exists) try {
        task(outFile)
      } catch {
        case t: Throwable =>
          if(outFile.exists) outFile.delete()
          throw t
      }
      outFile
    }
    def patchJarTask(task: TaskKey[File],
                     input: TaskKey[File], outputName: String, patchSection: String) =
      task := cachedFile(forge.forgeDir.value / outputName) { outFile =>
        val patchSet = BinPatch.readPatchSet(forge.binpatches.value, patchSection)
        BinPatch.patchJar(input.value, outFile, patchSet, streams.value.log)
      }

    def extractTask(task: TaskKey[File], urlSource: TaskKey[File], sourceName: String, outputName: String, targetDir: SettingKey[File]) =
      task := copyUrl(targetDir.value / outputName, jarFileUrl(urlSource.value, sourceName), streams.value.log, "Extracting")
    def downloadTask(task: TaskKey[File], versionKey: SettingKey[String], urlSource: TaskKey[String],
                     outputName: String, ext: String) =    
      task := copyUrl(forge.dlCacheDir.value / (outputName + "-" + versionKey.value + ext), urlSource.value, streams.value.log, "Downloading")

    val mappingRegex = "([^_]+)-(.+)".r
    def splitMapping(s: String) = s match {
      case mappingRegex(channel, version) => (channel, version)
      case _ => sys.error("Could not parse mapping channel name: "+s)
    }

    def dumpDebugMapping(debugDir: File, map: ForgeMapping, name: String) = {
      if(!debugDir.exists) debugDir.mkdirs()
      dumpMapping(new FileOutputStream(debugDir / name), map)
    }

    def ln(source: File, target: File) {
      import java.nio.file._
      Files.createSymbolicLink(Paths.get(target.getCanonicalPath), 
                               Paths.get(source.getCanonicalPath))
    }
  }
  import forgeHelpers._

  lazy val forgeSettingsBase: Seq[Setting[_]] = lwjglSettings ++ Seq(
    forge.fullVersion  := forge.mcVersion.value + "-" + forge.version.value,

    forge.cacheRoot  := target.value / "sbt-forge-cache",

    forge.buildDir   := forge.cacheRoot.value / "build",
    forge.forgeDir   := forge.buildDir.value / ("forge-"+forge.fullVersion.value),

    forge.ltCacheDir := forge.cacheRoot.value / "persistent",
    forge.dlCacheDir := forge.ltCacheDir.value / "downloads",
    forge.assetsDir  := forge.ltCacheDir.value / "assets",

    forge.runDir     := baseDirectory.value / "run",

    forge.cleanLtCache := false,
    forge.cleanRunDir  := false,

    forge.debugDir          := forge.buildDir.value / "debug",
    forge.debugDumpMappings := false,

    // Download needed files
    forge.clientDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/"+ver+".jar"),
    forge.serverDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/minecraft_server."+ver+".jar"),
    forge.universalDownloadUrl <<= defaultDownloadUrl("universal"),
    forge.userdevDownloadUrl   <<= defaultDownloadUrl("userdev"),
    forge.mappingDownloadUrl    := {
      val (ch, version) = splitMapping(forge.mappings.value)
      val ver = version + "-" + forge.mcVersion.value
      "http://files.minecraftforge.net/maven/de/oceanlabs/mcp/mcp_"+ch+"/"+ver+"/mcp_"+ch+"-"+ver+".zip"
    },

    downloadTask(forge.clientJar     , forge.mcVersion  , forge.clientDownloadUrl   , "minecraft_client", ".jar"),
    downloadTask(forge.serverJar     , forge.mcVersion  , forge.serverDownloadUrl   , "minecraft_server", ".jar"),
    downloadTask(forge.universalJar  , forge.fullVersion, forge.universalDownloadUrl, "forge_universal" , ".jar"),
    downloadTask(forge.userdevArchive, forge.fullVersion, forge.userdevDownloadUrl  , "forge_userdev"   , ".jar"),
    downloadTask(forge.mappingArchive, forge.mappings   , forge.mappingDownloadUrl  , "mcp_mappings"    , ".zip"),
    forge.assetsIndex := copyUrl(forge.assetsDir.value / "indexes" / (forge.mcVersion.value + ".json"), 
                                 "https://s3.amazonaws.com/Minecraft.Download/indexes/"+forge.mcVersion.value+".json", 
                                 streams.value.log, "Downloading"),

    // Extract files used in later processes
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

    forge.loadDependenciesFromJson :=
      (Json.parse(IO.read(forge.dependenciesJson.value)) \ "libraries").as[Seq[JsObject]].map { elem =>
        val Array(org, project, version) = (elem \ "name").as[String].split(":")
        org % project % version
      }.filter(x => !forge.excludedOrganizations.value.contains(x.organization)),
    // TODO: Add references to the repo versions of scala libraries.
    allDependencies <++= forge.loadDependenciesFromJson,

    // Process jars to merged SRG Forge binary
    forge.accessTransformers := Seq(
      copyUrl(forge.forgeDir.value / "forge_at.cfg", jarFileUrl(forge.userdevArchive.value, "src/main/resources/forge_at.cfg"), 
              streams.value.log, "Extracting"),
      copyUrl(forge.forgeDir.value / "fml_at.cfg"  , jarFileUrl(forge.userdevArchive.value, "src/main/resources/fml_at.cfg"  ), 
              streams.value.log, "Extracting")
    ),
    forge.mergedForgeBinary := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / "forgeBin_srg.jar") { outFile => 
        val classpath = (managedClasspath in Compile).value.map(_.data)
        val minecraftNotch = loadJarFile(new FileInputStream(forge.mergedJar.value))
        val forgeNotch = loadJarFile(new FileInputStream(forge.universalJar.value))
        val map = mappingFromSrgFile(minecraftNotch, IO.readLines(forge.srgFile.value), log)

        log.info("Adding mappings for Minecraft Forge inner classes")
        Renamer.findRemappableInnerClass(minecraftNotch.classes ++ forgeNotch.classes, map, log)

        log.info("Deobfing merged Minecraft binary to SRG names...")
        val (tmpmap_mc   , minecraftSrg) = Renamer.applyMapping(minecraftNotch, Seq(forge.universalJar.value), classpath, map, log)
        if(forge.debugDumpMappings.value) dumpDebugMapping(forge.debugDir.value, tmpmap_mc, "tmpmap_mc.sfmap")

        log.info("Restoring class attributes...")
        Exceptor.applyExceptorJson(minecraftSrg, IO.read(forge.exceptorJson.value), log)
        Exceptor.applyExcFile(minecraftSrg, new FileInputStream(forge.excFile.value), log)

        log.info("Deobfing Forge binary to SRG names...")
        val (tmpmap_forge, forgeSrg) = Renamer.applyMapping(forgeNotch, Seq(forge.mergedJar.value), classpath, map, log)
        if(forge.debugDumpMappings.value) dumpDebugMapping(forge.debugDir.value, tmpmap_forge, "tmpmap_forge.sfmap")

        log.info("Merging Forge binary and Minecraft binary...")
        val mergedBin = Merger.addForgeClasses(minecraftSrg, forgeSrg, log)

        log.info("Adding SRG parameter names...")
        Exceptor.addDefaultParameterNames(mergedBin)
        log.info("Stripping synthetic modifiers...")
        Exceptor.stripSynthetic(mergedBin)

        log.info("Removing patch data...")
        mergedBin.resources.remove("binpatches.pack.lzma")

        log.info("Running access transformers...")
        forge.accessTransformers.value.foreach { at =>
          log.info("  - "+at.getName)
          AccessTransformer.applyAccessTransformers(mergedBin, IO.readLines(at), log)
        }

        log.info("Writing merged Forge binary to "+outFile)
        writeJarFile(mergedBin, new FileOutputStream(outFile))
      }
    },

    forge.mappingCache := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / ("srg2mcp-"+forge.mappings.value+".sfmap")) { outFile => 
        log.info("Generating "+outFile+"...")
        val fieldsFile  = IO.readLinesURL(new URL(jarFileUrl(forge.mappingArchive.value, "fields.csv" ))).tail
        val methodsFile = IO.readLinesURL(new URL(jarFileUrl(forge.mappingArchive.value, "methods.csv"))).tail

        val map = mappingFromConfFiles(loadJarFile(new FileInputStream(forge.mergedForgeBinary.value)), fieldsFile, methodsFile)
        dumpMapping(new FileOutputStream(outFile), map)
      }
    },
    forge.forgeBinary := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / ("forgeBin-"+forge.fullVersion.value+"-"+forge.mappings.value+".jar")) { outFile => 
        log.info("Deobfing Forge binary to MCP names at "+outFile)
        val classpath = (managedClasspath in Compile).value.map(_.data)
        val map = readMapping(IO.readLines(forge.mappingCache.value))
        val (tmpmap_mcp, jar) = Renamer.applyMapping(loadJarFile(new FileInputStream(forge.mergedForgeBinary.value)), Seq(), classpath, map, log)
        if(forge.debugDumpMappings.value) dumpDebugMapping(forge.debugDir.value, tmpmap_mcp, "tmpmap_mcp.sfmap")

        log.info("Mapping parameter names...")
        val paramsFile = IO.readLinesURL(new URL(jarFileUrl(forge.mappingArchive.value, "params.csv"))).tail
        Renamer.mapParams(jar, paramsFile)

        writeJarFile(jar, new FileOutputStream(outFile))
      }
    },
    fullClasspath in Compile <+= forge.forgeBinary,

    forge.runOptions := ForkOptions(
      connectInput = true,
      outputStrategy = Some(StdoutOutput),
      runJVMOptions = javaOptions.value,
      workingDirectory = Some(forge.runDir.value)
    ),
    forge.prepareRunDir := {
      forge.runDir.value.mkdirs()

      val modDir = forge.runDir.value / "mods"
      val artifact = (artifactPath in (Compile, packageBin)).value

      IO.delete(modDir)
      modDir.mkdirs()
      ln(artifact, modDir / artifact.getName)
    },
    forge.downloadAssets := AssetManager.prepareAssets(forge.assetsDir.value, forge.assetsIndex.value, streams.value.log),
    forge.runClient in Runtime := {
      val runner = new ForkRun(forge.runOptions.value)
      val params = (Json.parse(IO.read(forge.dependenciesJson.value)) \ "minecraftArguments").as[String].split(" ")
      toError(runner.run(
        "net.minecraft.launchwrapper.Launch",
        (dependencyClasspath in Runtime).value.map(_.data) :+ forge.forgeBinary.value,
        params ++ Seq(
          "--username", "ForgeDevName",
          "--accessToken", "FML",
          "--gameDir", forge.runDir.value.getCanonicalPath,
          "--assetsDir", forge.assetsDir.value.getCanonicalPath,
          "--assetIndex", forge.mcVersion.value,
          "--userProperties", "{}"
        ),
        streams.value.log
      ))
    },
    forge.runServer in Runtime := {
      val runner = new ForkRun(forge.runOptions.value)
      toError(runner.run(
        "cpw.mods.fml.relauncher.ServerLaunchWrapper",
        (dependencyClasspath in Runtime).value.map(_.data) :+ forge.forgeBinary.value,
        Seq(),
        streams.value.log
      ))
    },

    forge.runClient in Runtime <<= (forge.runClient in Runtime) dependsOn 
      (Keys.`package` in Compile, forge.prepareRunDir, lwjgl.copyNatives, forge.downloadAssets),
    forge.runServer in Runtime <<= (forge.runServer in Runtime) dependsOn 
      (Keys.`package` in Compile, forge.prepareRunDir),

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
    forge.cleanCache := Defaults.doClean(Seq(forge.cacheRoot.value), Seq()),
    cleanKeepFiles ++= (if(forge.cleanLtCache.value) Seq() else Seq(forge.ltCacheDir.value)),
    cleanFiles ++= (if(forge.cleanRunDir.value) Seq(forge.cacheRoot.value, forge.runDir.value)
                    else                        Seq(forge.cacheRoot.value))
  )
  lazy val internalMergeClasses = Seq(
    extractTask(forge.binpatches, forge.universalJar, "binpatches.pack.lzma", "binpatches.pack.lzma", forge.forgeDir),
    patchJarTask(forge.patchClientJar, forge.clientJar, "minecraft_client_patched.jar", "client"),
    patchJarTask(forge.patchServerJar, forge.serverJar, "minecraft_server_patched.jar", "server"),
    forge.mergedJar := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / "minecraft_merged.jar") { outFile =>
        log.info("Merging patched client and server binaries to "+outFile)
        writeJarFile(Merger.merge(
          loadJarFile(new FileInputStream(forge.patchClientJar.value)),
          loadJarFile(new FileInputStream(forge.patchServerJar.value)),
          IO.readLines(forge.mergeConfig.value), log, forge.isRenamed.value
        ), new FileOutputStream(outFile))
      }
    }
  )
  lazy val forgeGradleMergeClasses = Seq(
    extractTask(forge.binpatches, forge.userdevArchive, "devbinpatches.pack.lzma", "devbinpatches.pack.lzma", forge.forgeDir),
    forge.fgMergeJars := {
      val log = streams.value.log
      cachedFile(forge.forgeDir.value / "minecraft_merged_fg.jar") { outFile =>
        log.info("Merging client and server binaries to "+outFile)
        Merger.forgeGradleMerge(forge.clientJar.value, forge.serverJar.value,
                                outFile, new FileInputStream(forge.mergeConfig.value),
                                forge.isRenamed.value)
      }
    },
    patchJarTask(forge.mergedJar, forge.fgMergeJars, "minecraft_merged.jar", "merged")
  )
  lazy val forgeSettings_1_7_10 = forgeSettingsBase ++ forgeGradleMergeClasses ++ Seq(
    forge.mcVersion := "1.7.10",
    forge.version   := "10.13.2.1291",
    forge.mappings  := "stable-12",
    scalaVersion    := "2.11.1",
    forge.isRenamed := false,

    forge.excludedOrganizations := Set("org.scala-lang", "org.scala-lang.plugins", "org.lwjgl.lwjgl")
  )
  lazy val forgeSettings_1_8 = forgeSettingsBase ++ forgeGradleMergeClasses ++ Seq(
    forge.mcVersion := "1.8",
    forge.version   := "11.14.0.1299",
    forge.mappings  := "stable-16",
    scalaVersion    := "2.11.1",
    forge.isRenamed := true,

    forge.excludedOrganizations := Set("org.scala-lang", "org.scala-lang.plugins", "org.lwjgl.lwjgl")
  )
}
