package moe.lymia.forge

import java.io._
import java.net.URL

import moe.lymia.forge.LWJGLPlugin.autoImport._
import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import moe.lymia.forge.build._
import moe.lymia.forge.launcher.MinecraftLauncher
import moe.lymia.forge.mapper.Renamer
import moe.lymia.forge.mapper.mapping._
import org.apache.commons.io.FileUtils
import play.api.libs.json._
import sbt.Keys._
import sbt.{Def, _}

// TODO: Remove mcBaseVersion, and instead load the MCP versions.json file.
// TODO: Put all the default URLs, etc into its own file.
// TODO: Find dependencies not available in Forge for the dependency shadowing system.
// TODO: Investigate how Ivy works in sbt on a lower level to support such.
// TODO: Reobf and deobf of mods.
// TODO: Work on mod dependencies system.
// TODO: Work on artifact publishing.
// TODO: Work on access transformer system.

object BaseForgePlugin extends AutoPlugin {
  object autoImport {
    // Configurations
    val Forge = config("Forge") extend Default describedAs
      "A configuration used to build Forge binaries."

    // Task/setting/input keys
    object forge {
      // User setting keys
      val mcBaseVersion = SettingKey[String]("forge-minecraft-base-version",
        "The base version of Minecraft. (e.g. 1.12 for 1.12.2. This is the version used to download the MCP mapping.")
      val mcVersion     = SettingKey[String]("forge-minecraft-version",
        "The version of Minecraft to build against.")
      val version       = SettingKey[String]("forge-version",
        "THe version of Minecraft Forge to build against.")
      val fullVersion   = SettingKey[String]("forge-full-version",
        "The Minecraft Forge version number combined with the Minecraft version number.")
      val scalaVersion  = SettingKey[String]("forge-scala-version",
        "The version of Scala that Minecraft Forge bundles.")

      val mappings      = SettingKey[String]("forge-mappings",
        "Mapping from SRG names to MCP names to use")

      val cacheRoot     = SettingKey[File]("forge-cache-root",
        "The root directory where all Forge related files are cached.")
      val depDir        = SettingKey[File]("forge-dep-dir",
        "Directory used to store dependency tracking information")

      val buildDir      = SettingKey[File]("forge-build-dir",
        "Directory used to store temporary build files used by sbt-forge")
      val forgeDir      = SettingKey[File]("forge-forge-dir",
        "Directory used to store files specific to a forge version")

      val ltCacheDir    = SettingKey[File]("forge-lt-dir",
        "Directory used to store persistant files generated by sbt-forge")
      val dlCacheDir    = SettingKey[File]("forge-download-dir",
        "Directory used to store files downloaded by sbt-forge")
      val launcherDir   = SettingKey[File]("forge-launcher-dir",
        "Directory used by the Minecraft launcher integrated into sbt-forge ")

      val runDir        = SettingKey[File]("forge-run-dir",
        "Directory used at runtime by Minecraft")

      val cleanLtCache  = SettingKey[Boolean]("forge-clean-download-cache",
        "If set, sbt clean will erase the long term cache directory, containing downloads and access tokens")
      val cleanRunDir   = SettingKey[Boolean]("forge-clean-run-dir",
        "If set, sbt clean will erase the runtime directory, containing Minecraft configuration and saves")

      // URL locations
      val universalDownloadUrl = TaskKey[String]("forge-universal-download-url",
        "Download URL for the Forge binary")
      val userdevDownloadUrl   = TaskKey[String]("forge-userdev-download-url",
        "Download URL for the Userdev archive")
      val srgDownloadUrl       = TaskKey[String]("forge-srg-download-url",
        "Download URL for Notch -> SRG mapping archive")
      val mappingDownloadUrl   = TaskKey[String]("forge-mapping-url",
        "Download URL for the SRG->MCP mappings")

      // Download needed files
      val clientJar      = TaskKey[File]("forge-client-jar",
        "Downloads the client jar.")
      val serverJar      = TaskKey[File]("forge-server-jar",
        "Downloads the server jar.")
      val universalJar   = TaskKey[File]("forge-universal-jar",
        "Downloads the Forge universal jar.")
      val userdevArchive = TaskKey[File]("forge-userdev-archive",
        "Downloads the Forge userdev archive.")
      val srgArchive     = TaskKey[File]("forge-srg-archive",
        "Downloads the MCP SRG data archive.")
      val mappingArchive = TaskKey[File]("forge-mapping-archive",
        "Downloads the MCP mapping archive.")

      // Extract files needed by later steps.
      val fieldsMapCsv     = TaskKey[File]("forge-fields-map-csv",
        "Extracts the .csv file containing SRG->MCP field name mappings.")
      val methodsMapCsv    = TaskKey[File]("forge-methods-map-csv",
        "Extracts the .csv file containing SRG->MCP method name mappings.")
      val paramsMapCsv     = TaskKey[File]("forge-params-map-csv",
        "Extracts the .csv file containing SRG->MCP parameter name mappings.")
      val mcpSrgFile       = TaskKey[File]("forge-mcp-srg-file",
        "Extracts the .srg file used for notch->SRG deobf.")
      val exceptorJson     = TaskKey[File]("forge-exceptor-json",
        "Extracts the .json file used to restore inner/outer class attributes to classes.")
      val mcpExcFile       = TaskKey[File]("forge-mcp-exc-file",
        "Extracts the .exc file used to restore exception data, and constructor parameter names to classes.")
      val dependenciesJson = TaskKey[File]("forge-dependencies-json",
        "Extracts the .json file declaring Minecraft's dependencies.")
      val binpatches       = TaskKey[File]("forge-binpatches",
        "Extracts the binary patches to patch Minecraft classes with.")
      val forgeAtFile      = TaskKey[File]("forge-forge-at-file",
        "Extracts the access transformer used by Minecraft Forge itself.")

      // Load forge's dependency .json file
      val excludedOrganizations         = SettingKey[Set[String]]("forge-excluded-organizations",
        "Organizations excluded from dependency autoloading")
      val minecraftAllProvidedLibraries = TaskKey[Seq[ModuleID]]("forge-all-minecraft-provided-libraries",
        "Libraries expected to be provided from Minecraft or Forge.")
      val minecraftProvidedLibraries    = TaskKey[Seq[ModuleID]]("forge-minecraft-provided-libraries",
        "Libraries expected to be provided from Minecraft or Forge, except those specified in excludedOrganizations.")

      // Patch and merge client .jars
      val serverDepPrefixes = TaskKey[Seq[String]]("forge-server-dep-prefixes",
        "A list of packages in the Minecraft server .jar that originate from dependencies, not Minecraft itself.")
      val patchedServerJar  = TaskKey[File]("forge-patched-server-jar",
        "Patched Minecraft server binary")
      val patchedClientJar  = TaskKey[File]("forge-patched-client-jar",
        "Patched Minecraft client binary")
      val mergedJar         = TaskKey[File]("forge-merged-jar",
        "Merged Minecraft binary")

      // Deobf merged .jar to SRG names
      val accessTransformers = TaskKey[Seq[File]]("forge-access-transformers",
        "List of access transformers to be applied to the Forge binary")
      val srgForgeBinary     = TaskKey[File]("forge-srg-forge-binary",
        "Deobfs and merges the Minecraft binary and the Minecraft Forge binary to SRG names")

      // Remap .jar files from SRG names to MCP names
      val mappingCache    = TaskKey[File]("forge-mapping-cache",
        "Cached mapping generated from .csv files")
      val srgToMcpMapping = TaskKey[File]("forge-srg-to-mcp-mapping",
        "Cached SRG to MCP mapping used by Forge")
      val forgeBinary     = TaskKey[File]("forge-binary",
        "Forge binary remapped to MCP names")

      // Run Minecraft
      val runOptions   = TaskKey[ForkOptions]("forge-run-options", "Fork options for running Minecraft")
      val modClasspath = TaskKey[Seq[File]]("forge-mod-classpath", "A list of mods loaded by runClient and runServer")
      val login        = InputKey[Unit]("login", "Logs you into a Minecraft account")
      val logout       = InputKey[Unit]("logout", "Logs you out of your Minecraft account")
      val runClient    = InputKey[Unit]("run-client", "Runs the Minecraft client.")
      val runServer    = InputKey[Unit]("run-server", "Runs the Minecraft server")

      // Clean all
      val cleanCache    = TaskKey[Unit]("clean-cache",
        "Cleans sbt-forge's long term cache")
    }
  }
  import autoImport._

  object forgeHelpers {
    def defaultDownloadUrl(ver: String, section: String) =
      s"http://files.minecraftforge.net/maven/net/minecraftforge/forge/$ver/forge-$ver-$section.jar"

    def jarFileUrl(jar: File, file: String) =
      s"jar:${jar.toURI.toURL}!/$file"

    def patchJarTask(task: TaskKey[File], inputTask: TaskKey[File],
                     outputName: String, patchSection: String) =
      task := {
        val (log, binpatches) = (streams.value.log, forge.binpatches.value)
        val cacheDir = forge.depDir.value / s"patch-jar_${forge.version.value}_${outputName.replace('.', '-')}"
        cachedTransform(cacheDir, inputTask.value, forge.forgeDir.value / outputName) { (input, outFile) =>
          val patchSet = BinPatch.readPatchSet(binpatches, patchSection)
          BinPatch.patchJar(input, outFile, patchSet, log)
        }
      }

    def extractTask(task: TaskKey[File], urlSource: TaskKey[File],
                    sourceName: String, outputName: String, isMappingBased: Boolean = false) =
      task := {
        val log = streams.value.log
        val mapVersion = if (isMappingBased) s"_${forge.mappings.value}" else ""
        val cacheDir =
          forge.depDir.value / s"extract_${forge.version.value}${mapVersion}_${outputName.replace('.', '-')}"
        cachedTransform(cacheDir, urlSource.value, forge.forgeDir.value / outputName) { (source, outFile) =>
          val jarUrl = jarFileUrl(source, sourceName)
          log.info(s"Extracting $jarUrl to $outFile")
          FileUtils.copyURLToFile(new URL(jarUrl), outFile)
        }
      }
    def downloadTask(task: TaskKey[File], versionKey: SettingKey[String], urlSource: TaskKey[String],
                     outputName: String, ext: String) =
      task := {
        val log = streams.value.log
        val url = urlSource.value
        cachedOperation(forge.dlCacheDir.value / s"$outputName-${versionKey.value}$ext") { outFile =>
          download(new URL(url), outFile, log)
        }
      }

    val mappingRegex = "([^_]+)_(.+)".r
    def splitMapping(s: String) = s match {
      case mappingRegex(channel, version) => (channel, version)
      case _ => sys.error(s"Could not parse mapping channel name: $s")
    }

    def dumpDebugMapping(debugDir: File, map: ForgeMapping, name: String) = {
      if(!debugDir.exists) debugDir.mkdirs()
      dumpMapping(new FileOutputStream(debugDir / name), map)
    }
  }
  import forgeHelpers._

  // Initialize Forge scopes
  private lazy val depsFromJar: Seq[Def.Setting[_]] = Seq(
    forge.minecraftProvidedLibraries := forge.minecraftAllProvidedLibraries.value
      .filter(x => !forge.excludedOrganizations.value.contains(x.organization)),
    allDependencies ++= forge.minecraftProvidedLibraries.value,
  )

  private lazy val forgeIvyCtx: Seq[Def.Setting[_]] = Classpaths.configSettings ++ Classpaths.ivyBaseSettings ++ Seq(
    allDependencies := Seq(),
    allDependencies ++= lwjgl.libraries.value,

    // We don't actually have any proper products. We use Classpaths only for downloading Maven dependencies.
    products := Seq(),
    exportedProducts := Seq(),
    exportedProductsIfMissing := Seq(),
    exportedProductsNoTracking := Seq(),
    exportedProductJars := Seq(),
    exportedProductJarsIfMissing := Seq(),
    exportedProductJarsNoTracking := Seq(),

    artifactPath := forge.cacheRoot.value / "artifact_path_keep_empty",
    classDirectory := forge.cacheRoot.value / "class_directory_keep_empty",
  ) ++ depsFromJar
  private lazy val projectSettingsCommon =
    depsFromJar ++ inConfig(Forge)(forgeIvyCtx)

  override val requires = LWJGLPlugin
  override lazy val projectSettings = projectSettingsCommon ++ Seq(
    forge.fullVersion  := forge.mcVersion.value + "-" + forge.version.value,

    forge.cacheRoot    := target.value / "sbt-forge-cache",
    forge.depDir       := forge.cacheRoot.value / "cache-info",

    forge.buildDir     := forge.cacheRoot.value / "build",
    forge.forgeDir     := forge.buildDir.value / ("forge-"+forge.fullVersion.value),

    forge.ltCacheDir   := forge.cacheRoot.value / "persistent",
    forge.dlCacheDir   := forge.ltCacheDir.value / "downloads",
    forge.launcherDir  := forge.ltCacheDir.value / "launcher",

    forge.runDir       := baseDirectory.value / "run",

    forge.cleanLtCache := false,
    forge.cleanRunDir  := false,

    // Download needed files
    forge.universalDownloadUrl := defaultDownloadUrl(forge.fullVersion.value, "universal"),
    forge.userdevDownloadUrl   := defaultDownloadUrl(forge.fullVersion.value, "userdev"),
    forge.srgDownloadUrl       := {
      val mcVersion = forge.mcVersion.value
      s"http://files.minecraftforge.net/maven/de/oceanlabs/mcp/mcp/$mcVersion/mcp-$mcVersion-srg.zip"
    },
    forge.mappingDownloadUrl   := {
      val (ch, version) = splitMapping(forge.mappings.value)
      val ver = version + "-" + forge.mcBaseVersion.value
      s"http://export.mcpbot.bspk.rs/mcp_$ch/$ver/mcp_$ch-$ver.zip"
    },

    forge.clientJar :=
      MinecraftLauncher.downloadClient(forge.launcherDir.value, forge.mcVersion.value, streams.value.log),
    forge.serverJar :=
      MinecraftLauncher.downloadServer(forge.launcherDir.value, forge.mcVersion.value, streams.value.log),

    downloadTask(forge.universalJar  , forge.fullVersion  , forge.universalDownloadUrl, "forge_universal" , ".jar"),
    downloadTask(forge.userdevArchive, forge.fullVersion  , forge.userdevDownloadUrl  , "forge_userdev"   , ".jar"),
    downloadTask(forge.srgArchive    , forge.mcBaseVersion, forge.srgDownloadUrl      , "mcp_srg_data"    , ".zip"),
    downloadTask(forge.mappingArchive, forge.mappings     , forge.mappingDownloadUrl  , "mcp_mappings"    , ".zip"),

    // Extract files used in later processes
    extractTask(forge.fieldsMapCsv    , forge.mappingArchive, "fields.csv"          , "mapping_fields.csv"   , true),
    extractTask(forge.methodsMapCsv   , forge.mappingArchive, "methods.csv"         , "mapping_methods.csv"  , true),
    extractTask(forge.paramsMapCsv    , forge.mappingArchive, "params.csv"          , "mapping_params.csv"   , true),
    extractTask(forge.mcpSrgFile      , forge.srgArchive    , "joined.srg"          , "srg_joined.srg"       ),
    extractTask(forge.exceptorJson    , forge.srgArchive    , "exceptor.json"       , "srg_exceptor.json"    ),
    extractTask(forge.mcpExcFile      , forge.srgArchive    , "joined.exc"          , "srg_joined.exc"       ),
    extractTask(forge.binpatches      , forge.universalJar  , "binpatches.pack.lzma", "binpatches.pack.lzma" ),
    extractTask(forge.dependenciesJson, forge.userdevArchive, "dev.json"            , "userdev_dev.json"     ),
    extractTask(forge.forgeAtFile     , forge.userdevArchive, "merged_at.cfg"       , "userdev_merged_at.cfg"),

    // Set up dependency resolution
    resolvers += "forge" at "http://files.minecraftforge.net/maven",
    resolvers += "minecraft" at "https://libraries.minecraft.net/",
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += Resolver.sonatypeRepo("snapshots"),

    forge.minecraftAllProvidedLibraries :=
      MinecraftLauncher.getDependencies(forge.launcherDir.value, forge.mcVersion.value,
                                        Seq(forge.dependenciesJson.value), streams.value.log),

    scalaVersion := forge.scalaVersion.value,

    // Patch and merge client and server jars.
    patchJarTask(forge.patchedClientJar, forge.clientJar, "minecraft_client_patched.jar", "client"),
    patchJarTask(forge.patchedServerJar, forge.serverJar, "minecraft_server_patched.jar", "server"),
    forge.mergedJar := {
      val log = streams.value.log
      val (patchedClientJar, patchedServerJar, universalJar, serverDepPrefixes) =
        (forge.patchedClientJar.value, forge.patchedServerJar.value,
         forge.universalJar.value, forge.serverDepPrefixes.value)
      val cacheDir = forge.depDir.value / s"merge-jar_${forge.version.value}"
      val outFile = forge.forgeDir.value / "minecraft_merged.jar"
      trackDependencies(cacheDir, Set(patchedClientJar, patchedServerJar, universalJar)) {
        log.info("Merging client and server binaries to "+outFile)
        writeJarFile(
          Merger.merge(patchedClientJar, patchedServerJar, universalJar, serverDepPrefixes, log),
          new FileOutputStream(outFile)
        )
        outFile
      }
    },

    // Process jars to merged SRG Forge binary
    forge.accessTransformers := Seq(),
    forge.accessTransformers in Forge := forge.accessTransformers.value :+ forge.forgeAtFile.value,
    forge.srgForgeBinary := {
      val log = streams.value.log

      val classpath = (fullClasspath in Forge).value.map(_.data)
      val (mergedJar, universalJar) = (forge.mergedJar.value, forge.universalJar.value)
      val (mcpSrgFile, exceptorJson, mcpExcFile) =
        (forge.mcpSrgFile.value, forge.exceptorJson.value, forge.mcpExcFile.value)
      val accessTransformers = (forge.accessTransformers in Forge).value

      val cacheDir = forge.depDir.value / s"srg-forge-binary_${forge.version.value}"
      val deps =
        Set(mergedJar, universalJar, mcpSrgFile, exceptorJson, mcpExcFile) ++
        accessTransformers.toSet ++ classpath.toSet
      val outFile = forge.forgeDir.value / "forgeBin_srg.jar"

      trackDependencies(cacheDir, deps) {
        val minecraftNotch = loadJarFile(new FileInputStream(mergedJar))
        val forgeNotch = loadJarFile(new FileInputStream(universalJar))
        val map = readSrgMapping(minecraftNotch, IO.readLines(mcpSrgFile), log)

        log.info("Adding mappings for Minecraft Forge inner classes")
        Renamer.findRemappableInnerClass(minecraftNotch.classes ++ forgeNotch.classes, map, log)

        log.info("Deobfing merged Minecraft binary to SRG names...")
        val (tmpmap_mc, minecraftSrg) =
          Renamer.applyMapping(minecraftNotch, Seq(universalJar), classpath, map, log)

        log.info("Restoring class attributes...")
        Exceptor.applyExceptorJson(minecraftSrg, IO.read(exceptorJson), log)
        Exceptor.applyExcFile(minecraftSrg, new FileInputStream(mcpExcFile), log)

        log.info("Removing snowmen...")
        Exceptor.stripSnowmen(minecraftSrg)

        log.info("Stripping synthetic modifiers...")
        Exceptor.stripSynthetic(minecraftSrg)

        log.info("Deobfing Forge binary to SRG names...")
        val (tmpmap_forge, forgeSrg) =
          Renamer.applyMapping(forgeNotch, Seq(mergedJar), classpath, map, log)

        log.info("Merging Forge binary and Minecraft binary...")
        val mergedBin = Merger.addForgeClasses(minecraftSrg, forgeSrg, log)

        log.info("Adding SRG parameter names...")
        Exceptor.addDefaultParameterNames(mergedBin)

        log.info("Removing patch data...")
        mergedBin.resources.remove("binpatches.pack.lzma")

        log.info("Running access transformers...")
        val transformer = AccessTransformer.parse(accessTransformers : _*)
        transformer.writeTo(new File("transformer_tmp_at.cfg"))
        val transformedBin = transformer.transformJar(mergedBin)

        log.info(s"Writing merged Forge binary to $outFile")
        writeJarFile(transformedBin, new FileOutputStream(outFile))
        outFile
      }
    },

    forge.mappingCache := {
      val log = streams.value.log

      val cacheDir = forge.depDir.value / s"mapping-cache_${forge.version.value}_${forge.mappings.value}"
      val (fieldsFile, methodsFile) = (forge.fieldsMapCsv.value, forge.methodsMapCsv.value)
      val srgForgeBinary = forge.srgForgeBinary.value
      val outFile = forge.forgeDir.value / s"srg2mcp-${forge.mappings.value}.sfmap"

      trackDependencies(cacheDir, Set(fieldsFile, methodsFile, srgForgeBinary)) {
        log.info(s"Generating $outFile...")
        val map = mappingFromConfFiles(loadJarFile(new FileInputStream(srgForgeBinary)),
                                       IO.readLines(fieldsFile).tail, IO.readLines(methodsFile).tail)
        dumpMapping(new FileOutputStream(outFile), map)
        outFile
      }
    },
    forge.srgToMcpMapping := {
      val log = streams.value.log

      val cacheDir = forge.depDir.value / s"srg2mcp-srg_${forge.version.value}_${forge.mappings.value}"
      val mappingCache = forge.mappingCache.value
      val outFile = forge.forgeDir.value / s"srg2mcp-${forge.mappings.value}.srg"

      cachedTransform(cacheDir, mappingCache, outFile) { (mappingCache, outFile) =>
        log.info(s"Generating $outFile...")
        val map = readMapping(IO.readLines(mappingCache))
        dumpSrgMapping(new FileOutputStream(outFile), map)
      }
    },
    forge.forgeBinary := {
      val log = streams.value.log

      val classpath = (fullClasspath in Forge).value.map(_.data)
      val mappingCache = forge.mappingCache.value
      val srgForgeBinary = forge.srgForgeBinary.value
      val paramsFile = forge.paramsMapCsv.value

      val cacheDir = forge.depDir.value / s"forge-binary_${forge.version.value}_${forge.mappings.value}"
      val deps = Set(mappingCache, srgForgeBinary, paramsFile) ++ classpath.toSet
      val outFile = forge.forgeDir.value / s"forgeBin-${forge.fullVersion.value}-${forge.mappings.value}.jar"

      trackDependencies(cacheDir, deps) {
        log.info(s"Deobfing Forge binary to MCP names at $outFile")
        val map = readMapping(IO.readLines(mappingCache))
        val (tmpmap_mcp, jar) =
          Renamer.applyMapping(loadJarFile(new FileInputStream(srgForgeBinary)), Seq(), classpath, map, log)

        log.info("Mapping parameter names...")
        Renamer.mapParams(jar, IO.readLines(paramsFile).tail)

        writeJarFile(jar, new FileOutputStream(outFile))
        outFile
      }
    },
    unmanagedClasspath in Compile += forge.forgeBinary.value,

    // Launcher bindings
    forge.modClasspath := Seq(),
    forge.modClasspath += (Keys.`package` in Compile).value,

    forge.login  := MinecraftLauncher.login (forge.launcherDir.value, streams.value.log),
    forge.logout := MinecraftLauncher.logout(forge.launcherDir.value, streams.value.log),

    forge.runOptions := ForkOptions()
      .withConnectInput(true)
      .withOutputStrategy(Some(StdoutOutput))
      .withRunJVMOptions((javaOptions.value ++ Seq(
        "-Dfml.ignoreInvalidMinecraftCertificates=true",
        s"-Dnet.minecraftforge.gradle.GradleStart.srg.srg-mcp=${forge.srgToMcpMapping.value}"
      )).toVector)
      .withWorkingDirectory(forge.runDir.value),
    forge.runClient := {
      lwjgl.copyNatives.value // Discard value, use this just to... well, copy the natives
      createDirectories(forge.runDir.value)
      MinecraftLauncher.prepareModsDirectory(forge.runDir.value, forge.modClasspath.value, streams.value.log)

      val runner = new ForkRun(forge.runOptions.value)
      val forgeArgs = (Json.parse(IO.read(forge.dependenciesJson.value)) \ "minecraftArguments").as[String].split(" ")
      val launcherArgs = MinecraftLauncher.prepareClientLaunch(forge.runDir.value, forge.launcherDir.value,
                                                               forge.mcVersion.value, streams.value.log)
      runner.run(
        "net.minecraft.launchwrapper.Launch",
        (fullClasspath in Forge).value.map(_.data) :+ forge.forgeBinary.value,
        forgeArgs ++ launcherArgs,
        streams.value.log
      ).get
    },
    forge.runServer := {
      createDirectories(forge.runDir.value)
      MinecraftLauncher.prepareModsDirectory(forge.runDir.value, forge.modClasspath.value, streams.value.log)

      val runner = new ForkRun(forge.runOptions.value)
      runner.run(
        "net.minecraftforge.fml.relauncher.ServerLaunchWrapper",
        (fullClasspath in Forge).value.map(_.data) :+ forge.forgeBinary.value,
        Seq(),
        streams.value.log
      ).get
    },

    forge.cleanCache := IO.delete(forge.cacheRoot.value),
    cleanKeepFiles ++= (if(forge.cleanLtCache.value) Seq() else Seq(forge.ltCacheDir.value)),
    cleanFiles ++= (if(forge.cleanRunDir.value) Seq(forge.cacheRoot.value, forge.runDir.value)
                    else                        Seq(forge.cacheRoot.value))
  )
}

object ForgePlugin_1_12 extends AutoPlugin {
  override def requires = BaseForgePlugin

  import BaseForgePlugin.autoImport._

  override def projectSettings = Seq(
    forge.mcBaseVersion := "1.12",
    forge.mcVersion     := "1.12.2",
    forge.version       := "14.23.4.2759",
    forge.mappings      := "stable_39",

    forge.scalaVersion  := "2.11.1",
    lwjgl.version       := "2.9.4-nightly-20150209",

    forge.excludedOrganizations := Set("org.scala-lang", "org.scala-lang.modules", "org.lwjgl.lwjgl"),
    forge.excludedOrganizations in Forge := Set("org.lwjgl.lwjgl"),
    forge.serverDepPrefixes := Seq(
      "org/bouncycastle/", "org/apache/", "com/google/", "com/mojang/authlib/", "com/mojang/util/",
      "gnu/trove/", "io/netty/", "javax/annotation/", "argo/", "it/"
    )
  )
}