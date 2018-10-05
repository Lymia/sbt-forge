package moe.lymia.forge

import java.io._
import java.net.URL

import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import moe.lymia.forge.build._
import moe.lymia.forge.launcher.MinecraftLauncher
import moe.lymia.forge.mapper._
import org.apache.commons.io.FileUtils
import play.api.libs.json._
import sbt.Keys._
import sbt.{Def, _}

// TODO: Put all the default URLs, etc into its own file.
// TODO: Reobf and deobf of mods.
// TODO: Work on artifact publishing.
// TODO: Add Tags to our tasks.
// TODO: Make sure the plugin works well in multi-project builds.
// TODO: Implement extraction of dependencies from the classpath.
// TODO: Consider integrating the recently extracted ForgeGradle 3.0 libraries?
// TODO: Apply the SRG/EXC/etc files found in the userdev archive.
// TODO: Consider sbt 0.x support.
// TODO: Figure out builds for multiple Minecraft versions (use multi-version SBT plugins as a template?)

object BaseForgePlugin extends AutoPlugin {
  object autoImport {
    // Configurations
    val Extract = config("extract").hide describedAs "Dependencies that should be extracted from the mod jar."
    val Shade = config("shade").hide describedAs "Dependencies that should be shaded into the mod jar."
    val ShadeDeps = config("shadeDeps").hide describedAs "Dependencies that should be shaded into the mod jar transitively."

    // User setting keys
    val forgeVersion    = settingKey[String]("The version of Minecraft Forge to build against.")
    val mappingChannel  = settingKey[String]("Mapping from SRG names to MCP names to use")

    val forgeBuildCache    = settingKey[File]("The root directory where Forge build files are cached.")
    val forgeDownloadCache = settingKey[File]("Directory used to store files downloaded by sbt-forge")
    val minecraftHome      = settingKey[File]("Directory used at runtime by Minecraft")

    val cleanDownloads     = settingKey[Boolean]("Whether to clean the sbt-forge download cache.")
    val cleanMinecraftHome = settingKey[Boolean]("Whether to clean up the Minecraft home directory.")

    val shadePrefix     = settingKey[String]("The package to move shaded dependencies into.")
    val shadeScalaLibs  = settingKey[Boolean]("Whether to shade scala-library.")
    val autoExtractDeps = settingKey[Boolean]("Whether to extract dependencies by default.")

    // User task keys
    val minecraftClasspath   = taskKey[Seq[ModuleID]]("Libraries expected to be provided from Minecraft or Forge.")
    val accessTransformers   = taskKey[Seq[File]]("List of access transformers to be applied to the Forge binary")
    val minecraftForkOptions = taskKey[ForkOptions]("Fork options for running Minecraft")

    // Run Minecraft
    val login        = inputKey[Unit]("Logs you into a Minecraft account")
    val logout       = inputKey[Unit]("Logs you out of your Minecraft account")
    val runClient    = inputKey[Unit]("Runs the Minecraft client.")
    val runServer    = inputKey[Unit]("Runs the Minecraft server")

    object BaseForgePluginInternalKeys {
      // Internal configurations
      val Forge = config("forge").hide describedAs "Dependencies of Forge itself."
      val ForgeCompile = config("compile") extend (Forge, Extract, Shade, ShadeDeps, Optional)

      // Configuration for the current Minecraft version. Should be set by the ForgePlugin_1_X object.
      val forgeMcMappingVersion = settingKey[String]("The Minecraft version number used to find the MCP mapping.")
      val forgeMcVersion        = settingKey[String]("The version of Minecraft to build against.")
      val forgeFullVersion      = settingKey[String]("The full Minecraft Forge version.")
      val forgeScalaVersion     = settingKey[String]("The version of Scala that Minecraft Forge bundles.")

      val forgeExcludedOrganizations = settingKey[Set[String]]("Organizations to exclude from Compile.")
      val forgeServerDepPrefixes     = settingKey[Seq[String]]("A list of packages Minecraft shades.")

      // Paths used by the Minecraft Forge build
      val forgeDepDir   = settingKey[File]("Directory used to store dependency tracking information")
      val forgeBuildDir = settingKey[File]("Directory used to store files specific to a forge version")

      val forgeArtifactDir = settingKey[File]("Directory used to artifacts relating to Forge or MCP")
      val forgeLauncherDir = settingKey[File]("Directory used by the Minecraft launcher integrated into sbt-forge")

      // URL locations
      val forgeUniversalJarUrl = settingKey[String]("Download URL for the Forge binary")
      val forgeUserdevJarUrl   = settingKey[String]("Download URL for the Userdev archive")
      val forgeSrgZipUrl       = settingKey[String]("Download URL for Notch -> SRG mapping archive")
      val forgeMappingZipUrl   = settingKey[String]("Download URL for the SRG->MCP mappings")

      // Download needed files
      val forgeMcClientJar  = taskKey[File]("Downloads the client jar.")
      val forgeMcServerJar  = taskKey[File]("Downloads the server jar.")
      val forgeUniversalJar = taskKey[File]("Downloads the Forge universal jar.")
      val forgeUserdevJar   = taskKey[File]("Downloads the Forge userdev archive.")
      val forgeSrgZip       = taskKey[File]("Downloads the MCP SRG data archive.")
      val forgeMappingZip   = taskKey[File]("Downloads the MCP mapping archive.")

      // Extract files needed by later steps.
      val forgeFieldsMap    = taskKey[File]("Extracts the .csv file containing SRG->MCP field name mappings.")
      val forgeMethodsMap   = taskKey[File]("Extracts the .csv file containing SRG->MCP method name mappings.")
      val forgeParamsMap    = taskKey[File]("Extracts the .csv file containing SRG->MCP parameter name mappings.")
      val forgeSrgMap       = taskKey[File]("Extracts the .srg file used for notch->SRG deobf.")
      val forgeExceptorJson = taskKey[File]("Extracts the .json file used to restore inner class attributes.")
      val forgeMcpExcFile   = taskKey[File]("Extracts the .exc file used to restore exception attributes.")
      val forgeDepsJson     = taskKey[File]("Extracts the .json file declaring Forge's dependencies.")
      val forgeBinpatches   = taskKey[File]("Extracts the binary patches to patch Minecraft classes with.")
      val forgeUserdevAt    = taskKey[File]("Extracts the access transformer used by Minecraft Forge itself.")

      // Setup dependency resolution
      val forgeResolutionModuleId = settingKey[ModuleID]("The module ID used in the resolution cache by Forge.")

      // Build development Forge binary
      val forgePatchedServerJar = taskKey[File]("Patched Minecraft server binary")
      val forgePatchedClientJar = taskKey[File]("Patched Minecraft client binary")
      val forgeMcMerged         = taskKey[File]("Patched and merged Minecraft binary")
      val forgeSrgBinary        = taskKey[File]("SRG named Minecraft Forge universal binary")

      // Apply the mapping and access transformers requested by the user
      val forgeMappingCache    = taskKey[File]("Cached SRG to MCP mapping")
      val forgeRevMappingCache = taskKey[File]("Cached MCP to SRG mapping")
      val forgeMappingSrg      = taskKey[File]("Cached SRG to MCP mapping used by Forge")
      val forgeMcpBinary       = taskKey[File]("Forge binary remapped to MCP names")
      val forgeUserAtFile      = taskKey[String]("The name of the merged access transformer file")
      val forgeAtMcpBinary     = taskKey[File]("Forge binary with user access transformers applied")

      // Dependency shading
      val forgeShadeInfo       = taskKey[ShadeInfo]("Calculate which dependencies should be shaded or extracted.")
      val forgeDevShadedDepJar = taskKey[File]("Shade all dependencies into a single .jar for faster merging")
      val forgeDevShadedJar    = taskKey[File]("The MCP named mod .jar with dependencies shaded into it")

      // Mod classpaths
      val forgeDevModClasspath = taskKey[Classpath]("A list of mods loaded by runClient and runServer")
    }

    implicit class CurseForgeResolverExtension(resolver: Resolver.type) {
      val CurseForgeRepositoryName = "CurseForge"
      val CurseForgeRepositoryRoot = "https://minecraft.curseforge.com/api/maven/"
      val CurseForgeRepository = CurseForgeRepositoryName at CurseForgeRepositoryRoot

      val MinecraftRepositoryName = "Minecraft Maven repository"
      val MinecraftRepositoryRoot = "https://libraries.minecraft.net/"
      val MinecraftRepository = MinecraftRepositoryName at MinecraftRepositoryRoot

      val MinecraftForgeRepositoryName = "Minecraft Forge Maven repository"
      val MinecraftForgeRepositoryRoot = "http://files.minecraftforge.net/maven"
      val MinecraftForgeRepository = MinecraftForgeRepositoryName at MinecraftForgeRepositoryRoot
    }
  }
  import autoImport._
  import BaseForgePluginInternalKeys._
  import LWJGLNativesPlugin.autoImport.LWJGLNativesInternalKeys._

  // Various helper functions
  private def forgeDownloadUrl(ver: String, section: String) =
    s"http://files.minecraftforge.net/maven/net/minecraftforge/forge/$ver/forge-$ver-$section.jar"

  private def patchJarTask(task: TaskKey[File], inputTask: TaskKey[File],
                           outputName: String, patchSection: String) =
    task := {
      val (log, binpatches) = (streams.value.log, forgeBinpatches.value)
      val cacheDir = forgeDepDir.value / s"patch-jar_${forgeVersion.value}_${outputName.replace('.', '-')}"
      val excludedPrefixes = forgeServerDepPrefixes.value
      cachedTransform(cacheDir, inputTask.value, forgeBuildDir.value / outputName) { (input, outFile) =>
        val patchSet = BinPatch.readPatchSet(binpatches, patchSection)
        BinPatch.patchJar(input, outFile, patchSet, excludedPrefixes, log)
      }
    }

  private def extractTask(task: TaskKey[File], urlSource: TaskKey[File],
                          sourceName: String, outputName: String,
                          isMappingBased: Boolean = false) =
    task := {
      val log = streams.value.log
      val mapVersion = if (isMappingBased) s"_${mappingChannel.value}" else ""
      val cacheDir =
        forgeDepDir.value / s"extract_${forgeVersion.value}${mapVersion}_${outputName.replace('.', '-')}"
      cachedTransform(cacheDir, urlSource.value, forgeBuildDir.value / outputName) { (source, outFile) =>
        val jarUrl = jarFileUrl(source, sourceName)
        log.info(s"Extracting $jarUrl to $outFile")
        FileUtils.copyURLToFile(jarUrl, outFile)
      }
    }
  private def downloadTask(task: TaskKey[File], versionKey: SettingKey[String], urlSource: SettingKey[String],
                           outputName: String, ext: String) =
    task := {
      val log = streams.value.log
      val url = urlSource.value
      cachedOperation(forgeArtifactDir.value / s"$outputName-${versionKey.value}$ext") { outFile =>
        download(new URL(url), outFile, log)
      }
    }

  private val mappingRegex = "([^_]+)_(.+)".r
  private def splitMapping(s: String) = s match {
    case mappingRegex(channel, version) => (channel, version)
    case _ => sys.error(s"Could not parse mapping channel name: $s")
  }

  // Initialize Forge scopes
  private lazy val simpleIvyCtx: Seq[Def.Setting[_]] =
    Classpaths.configSettings ++ Classpaths.ivyBaseSettings ++ Seq(
      allDependencies := Seq(),
      moduleSettings :=
        ModuleDescriptorConfiguration(forgeResolutionModuleId.value, ModuleInfo(forgeResolutionModuleId.value.name))
          .withValidate(ivyValidate.value)
          .withScalaModuleInfo(scalaModuleInfo.value)
          .withDependencies(allDependencies.value.toVector)
          .withOverrides(dependencyOverrides.value.toVector)
          .withExcludes(excludeDependencies.value.toVector)
          .withIvyXML(ivyXML.value)
          .withConfigurations(ivyConfigurations.value.toVector)
          .withDefaultConfiguration(defaultConfiguration.value)
          .withConflictManager(conflictManager.value),

      // We don't actually have any proper products. We use Classpaths only for downloading Maven dependencies.
      products := Seq(),
      exportedProducts := Seq(),
      exportedProductsIfMissing := Seq(),
      exportedProductsNoTracking := Seq(),
      exportedProductJars := Seq(),
      exportedProductJarsIfMissing := Seq(),
      exportedProductJarsNoTracking := Seq(),

      artifactPath := forgeBuildCache.value / "artifact_path_keep_empty",
      classDirectory := forgeBuildCache.value / "class_directory_keep_empty",
    )
  private lazy val initScopes = inConfig(Forge)(simpleIvyCtx)

  override val requires = LWJGLNativesPlugin
  override lazy val projectSettings = initScopes ++ Seq(
    // Various default settings
    forgeFullVersion   := forgeMcVersion.value + "-" + forgeVersion.value,
    forgeBuildCache    := target.value / "sbt-forge-build-cache",
    forgeDepDir        := forgeBuildCache.value / "dep-tracking",
    forgeBuildDir      := forgeBuildCache.value / "minecraft-forge" / forgeMcVersion.value,

    forgeDownloadCache := target.value / "sbt-forge-downloads",
    forgeArtifactDir   := forgeDownloadCache.value / "artifacts",
    forgeLauncherDir   := forgeDownloadCache.value / "launcher",

    minecraftHome      := baseDirectory.value / "run",

    cleanDownloads     := false,
    cleanMinecraftHome := false,

    shadePrefix        := s"moe.lymia.shadeddeps.${Hash.toHex(Hash.apply(s"${organization.value}:${name.value}:${version.value}"))}",
    shadeScalaLibs     := CrossVersion.binaryScalaVersion(forgeScalaVersion.value) != scalaBinaryVersion.value,
    autoExtractDeps    := true,
    accessTransformers := Seq(),

    crossPaths         := false,

    // Download needed files
    forgeUniversalJarUrl := forgeDownloadUrl(forgeFullVersion.value, "universal"),
    forgeUserdevJarUrl   := forgeDownloadUrl(forgeFullVersion.value, "userdev"),
    forgeSrgZipUrl       := {
      val mcVersion = forgeMcVersion.value
      s"http://files.minecraftforge.net/maven/de/oceanlabs/mcp/mcp/$mcVersion/mcp-$mcVersion-srg.zip"
    },
    forgeMappingZipUrl   := {
      val (ch, version) = splitMapping(mappingChannel.value)
      val ver = version + "-" + forgeMcMappingVersion.value
      s"http://export.mcpbot.bspk.rs/mcp_$ch/$ver/mcp_$ch-$ver.zip"
    },

    forgeMcClientJar := MinecraftLauncher.downloadClient(forgeLauncherDir.value, forgeMcVersion.value, streams.value.log),
    forgeMcServerJar := MinecraftLauncher.downloadServer(forgeLauncherDir.value, forgeMcVersion.value, streams.value.log),

    downloadTask(forgeUniversalJar, forgeFullVersion     , forgeUniversalJarUrl, "forge_universal" , ".jar"),
    downloadTask(forgeUserdevJar  , forgeFullVersion     , forgeUserdevJarUrl  , "forge_userdev"   , ".jar"),
    downloadTask(forgeSrgZip      , forgeMcMappingVersion, forgeSrgZipUrl      , "mcp_srg_data"    , ".zip"),
    downloadTask(forgeMappingZip  , mappingChannel       , forgeMappingZipUrl  , "mcp_mappings"    , ".zip"),

    // Extract files used in later processes
    extractTask(forgeFieldsMap   , forgeMappingZip  , "fields.csv"          , "mapping_fields.csv"   , true),
    extractTask(forgeMethodsMap  , forgeMappingZip  , "methods.csv"         , "mapping_methods.csv"  , true),
    extractTask(forgeParamsMap   , forgeMappingZip  , "params.csv"          , "mapping_params.csv"   , true),
    extractTask(forgeSrgMap      , forgeSrgZip      , "joined.srg"          , "srg_joined.srg"       ),
    extractTask(forgeExceptorJson, forgeSrgZip      , "exceptor.json"       , "srg_exceptor.json"    ),
    extractTask(forgeMcpExcFile  , forgeSrgZip      , "joined.exc"          , "srg_joined.exc"       ),
    extractTask(forgeBinpatches  , forgeUniversalJar, "binpatches.pack.lzma", "binpatches.pack.lzma" ),
    extractTask(forgeDepsJson    , forgeUserdevJar  , "dev.json"            , "userdev_dev.json"     ),
    extractTask(forgeUserdevAt   , forgeUserdevJar  , "merged_at.cfg"       , "userdev_merged_at.cfg"),

    // Set up dependency resolution for Forge
    resolvers += Resolver.MinecraftForgeRepository,
    resolvers += Resolver.MinecraftRepository,
    resolvers in Forge := Seq(Resolver.MinecraftForgeRepository, Resolver.MinecraftRepository),

    ivyConfigurations :=
      overrideConfigs(Forge, Extract, Shade, ShadeDeps, ForgeCompile)(ivyConfigurations.value),

    forgeResolutionModuleId := "net.minecraftforge" % "forge" % forgeVersion.value,
    scalaModuleInfo in Forge := {
      val scalaVersion = forgeScalaVersion.value
      scalaModuleInfo.value.map(_.withScalaOrganization("org.scala-lang")
                                 .withScalaFullVersion(scalaVersion)
                                 .withScalaBinaryVersion(CrossVersion.binaryScalaVersion(scalaVersion)))
    },

    minecraftClasspath := MinecraftLauncher.getDependencies(forgeLauncherDir.value, forgeMcVersion.value,
                                                            Seq(forgeDepsJson.value), streams.value.log),
    allDependencies ++= {
      val excluded = forgeExcludedOrganizations.value
      minecraftClasspath.value.filter(x => !excluded.contains(x.organization)).map(_ % "forge")
    },
    allDependencies ++= lwjglNativeDeps.value.map(_ % "forge"),
    allDependencies in Forge ++= minecraftClasspath.value,

    scalaVersion := forgeScalaVersion.value,

    // Patch and merge client and server jars.
    patchJarTask(forgePatchedClientJar, forgeMcClientJar, "minecraft_client_patched.jar", "client"),
    patchJarTask(forgePatchedServerJar, forgeMcServerJar, "minecraft_server_patched.jar", "server"),
    forgeMcMerged := {
      val log = streams.value.log
      val (patchedClientJar, patchedServerJar) =
        (forgePatchedClientJar.value, forgePatchedServerJar.value)
      val cacheDir = forgeDepDir.value / s"merge-jar_${forgeVersion.value}"
      val outFile = forgeBuildDir.value / "minecraft_merged.jar"
      trackDependencies(cacheDir, Set(patchedClientJar, patchedServerJar)) {
        log.info("Merging client and server binaries to "+outFile)
        Merger.merge(patchedClientJar, patchedServerJar, log).write(outFile)
        outFile
      }
    },

    // Process jars to merged SRG Forge binary
    accessTransformers in Forge := Seq(forgeUserdevAt.value),
    forgeSrgBinary := {
      val log = streams.value.log

      val classpath = (fullClasspath in Forge).value.map(_.data)
      val (mergedJar, universalJar) = (forgeMcMerged.value, forgeUniversalJar.value)
      val (srgMap, exceptorJson, mcpExcFile) =
        (forgeSrgMap.value, forgeExceptorJson.value, forgeMcpExcFile.value)
      val forgeAccessTransformers = (accessTransformers in Forge).value

      val cacheDir = forgeDepDir.value / s"srg-forge-binary_${forgeVersion.value}"
      val deps =
        Set(mergedJar, universalJar, srgMap, exceptorJson, mcpExcFile) ++
        forgeAccessTransformers.toSet ++ classpath.toSet
      val outFile = forgeBuildDir.value / "forgeBin_srg.jar"

      trackDependencies(cacheDir, deps) {
        val minecraftNotch = JarData.load(mergedJar)
        val forgeNotch = JarData.load(universalJar).stripSignatures

        log.info("Removing snowmen...")
        Exceptor.stripSnowmen(minecraftNotch)

        log.info("Stripping synthetic modifiers...")
        Exceptor.stripSynthetic(minecraftNotch)

        log.info("Merging Forge binary and Minecraft binary...")
        val mergedNotch = minecraftNotch.mergeWith(forgeNotch, log, newIdentity = outFile.getName)
        val srgMapData = Mapping.readSrgMapping(mergedNotch, srgMap, log).findRemappableInnerClass(mergedNotch, log)

        log.info("Deobfing merged binary to SRG names...")
        val (_, mergedSrg) =
          JarRemapper.applyMapping(mergedNotch, Seq(), classpath, srgMapData, log)

        log.info("Restoring class attributes...")
        Exceptor.applyExceptorJson(mergedSrg, IO.read(exceptorJson), log)
        Exceptor.applyExcFile(mergedSrg, new FileInputStream(mcpExcFile), log)

        log.info("Adding SRG parameter names...")
        Exceptor.addDefaultParameterNames(mergedSrg)

        log.info("Removing patch data...")
        mergedSrg.resources.remove("binpatches.pack.lzma")

        log.info("Running Forge access transformers...")
        val transformedBin = AccessTransformer.parse(forgeAccessTransformers : _*).transformJar(mergedSrg)

        log.info(s"Writing merged Forge binary to $outFile")
        transformedBin.write(outFile)
        outFile
      }
    },

    forgeMappingCache := {
      val log = streams.value.log

      val cacheDir = forgeDepDir.value / s"srg2mcp-sfmap_${forgeVersion.value}_${mappingChannel.value}"
      val (fieldsFile, methodsFile) = (forgeFieldsMap.value, forgeMethodsMap.value)
      val srgBinary = forgeSrgBinary.value
      val outFile = forgeBuildDir.value / s"srg2mcp-${mappingChannel.value}.json"

      trackDependencies(cacheDir, Set(fieldsFile, methodsFile, srgBinary)) {
        log.info(s"Generating $outFile...")
        val map = Mapping.readMcpMapping(JarData.load(srgBinary), fieldsFile, methodsFile)
        map.writeCachedMapping(outFile)
        outFile
      }
    },
    forgeRevMappingCache := {
      val log = streams.value.log

      val cacheDir = forgeDepDir.value / s"mcp2srg-sfmap_${forgeVersion.value}_${mappingChannel.value}"
      val mappingCache = forgeMappingCache.value
      val outFile = forgeBuildDir.value / s"mcp2srg-${mappingChannel.value}.json"

      cachedTransform(cacheDir, mappingCache, outFile) { (mappingCache, outFile) =>
        log.info(s"Generating $outFile...")
        val map = Mapping.readCachedMapping(mappingCache).reverseMapping()
        map.writeCachedMapping(outFile)
      }
    },
    forgeMappingSrg := {
      val log = streams.value.log

      val cacheDir = forgeDepDir.value / s"srg2mcp-srg_${forgeVersion.value}_${mappingChannel.value}"
      val mappingCache = forgeMappingCache.value
      val outFile = forgeBuildDir.value / s"srg2mcp-${mappingChannel.value}.srg"

      cachedTransform(cacheDir, mappingCache, outFile) { (mappingCache, outFile) =>
        log.info(s"Generating $outFile...")
        val map = Mapping.readCachedMapping(mappingCache)
        map.writeSrgMapping(outFile)
      }
    },
    forgeMcpBinary := {
      val log = streams.value.log

      val classpath = (fullClasspath in Forge).value.map(_.data)
      val mappingCache = forgeMappingCache.value
      val srgBinary = forgeSrgBinary.value
      val paramsMap = forgeParamsMap.value

      val cacheDir = forgeDepDir.value / s"forge-binary_${forgeVersion.value}_${mappingChannel.value}"
      val deps = Set(mappingCache, srgBinary, paramsMap) ++ classpath.toSet
      val outFile = forgeBuildDir.value / s"forgeBin-${forgeFullVersion.value}-${mappingChannel.value}.jar"

      trackDependencies(cacheDir, deps) {
        log.info(s"Deobfing Forge binary to MCP names at $outFile")
        val map = Mapping.readCachedMapping(mappingCache)
        val (tmpmap_mcp, jar) = JarRemapper.applyMapping(JarData.load(srgBinary), Seq(), classpath, map, log)

        log.info("Mapping parameter names...")
        JarRemapper.mapParams(jar, paramsMap)

        jar.write(outFile)
        outFile
      }
    },

    // Apply user access transformers to the Forge binary. Note that these are based on MCP names.
    forgeAtMcpBinary := {
      val log = streams.value.log

      val cacheDir = forgeDepDir.value / s"forge-at-binary_${forgeVersion.value}_${mappingChannel.value}"
      val mcpBinary = forgeMcpBinary.value
      val userAccessTransformers = accessTransformers.value
      val outFile = forgeBuildDir.value / s"forgeBin-at-${forgeFullVersion.value}-${mappingChannel.value}.jar"

      trackDependencies(cacheDir, userAccessTransformers.toSet + mcpBinary) {
        log.info("Applying user access transformers...")
        val jar = JarData.load(mcpBinary)
        val atJar = AccessTransformer.parse(userAccessTransformers : _*).transformJar(jar)
        atJar.write(outFile)
        outFile
      }
    },
    forgeUserAtFile := s"${moduleName.value}_at.cfg",
    packageOptions in (Compile, packageBin) += Package.ManifestAttributes("FMLAT" -> forgeUserAtFile.value),
    resourceGenerators in Compile += Def.task {
      val at = AccessTransformer.parse(accessTransformers.value : _*)
      val target = (resourceManaged in Compile).value / "META-INF" / forgeUserAtFile.value
      at.remap(Mapping.readCachedMapping(forgeRevMappingCache.value)).writeTo(target)
      Seq(target)
    }.taskValue,
    unmanagedClasspath in Compile +=
      Attributed(forgeAtMcpBinary.value)
                (AttributeMap.empty.put(moduleID.key, forgeResolutionModuleId.value % Forge)),

    // Add dependency extraction related information to mod .jar
    packageOptions in (Compile, packageBin) += Package.ManifestAttributes(
      "Maven-Artifact" -> projectID.value.toString,
      "Timestamp" -> System.currentTimeMillis.toString
    ),

    // Shade dependencies into the mod .jar
    allDependencies := {
      val (allDeps, scalaOrg, scalaHome, scalaVersion) =
        (allDependencies.value, scalaOrganization.value, Keys.scalaHome.value, Keys.scalaVersion.value)
      val scalaSet = if (!autoScalaLibrary.value) allDeps else {
        if (scalaHome.isDefined) sys.error("shadeScalaLibs current does not work with scalaHome.")
        val target = scalaOrg % "scala-library" % scalaVersion
        val scalaRemoved = allDeps.filter(x => cleanModuleID(x) != target)
        scalaRemoved.map(x =>
          if (x.configurations.exists(_.contains("scala-tool"))) x.withConfigurations(Some("scala-tool;provided"))
          else x
        ) :+ (if (shadeScalaLibs.value) target % "scala-tool;shadeDeps" else target % "scala-tool;forge")
     }
      if (autoExtractDeps.value) scalaSet.map(x => if (x.configurations.isEmpty) x % Extract else x) else scalaSet
    },
    forgeShadeInfo := new ShadeInfo(allDependencies.value, update.value,
                                    (dependencyClasspath in Compile).value, shadePrefix.value),
    forgeDevShadedDepJar := {
      val log = streams.value.log

      val cacheDir = forgeDepDir.value / "shaded-dep-jar"
      val classpaths = forgeShadeInfo.value.deobfClasspaths
      val outFile = crossTarget.value / appendToFilename((packageBin in Compile).value.getName, "_deps")

      trackDependencies(cacheDir, classpaths.trackFiles, extra = classpaths.trackExtra) {
        log.info(s"Shading mod dependencies to $outFile...")
        val shadedJar = DepShader.generateDepsJar(classpaths, log)
        shadedJar.write(outFile)
        outFile
      }
    },
    forgeDevShadedJar := {
      val log = streams.value.log
      val modJar = (packageBin in Compile).value
      val outFile = crossTarget.value / appendToFilename(modJar.getName, "_shaded")
      log.info(s"Writing shaded mod jar to $outFile")
      DepShader.addDepsToJar(modJar, forgeDevShadedDepJar.value).write(outFile)
      outFile
    },

    // Launcher bindings
    forgeDevModClasspath := Seq(),
    forgeDevModClasspath += forgeDevShadedJar.value,
    forgeDevModClasspath ++= forgeShadeInfo.value.deobfClasspaths.modClasspath,

    login  := MinecraftLauncher.login (forgeLauncherDir.value, streams.value.log),
    logout := MinecraftLauncher.logout(forgeLauncherDir.value, streams.value.log),

    minecraftForkOptions := ForkOptions()
      .withConnectInput(true)
      .withOutputStrategy(Some(StdoutOutput))
      .withRunJVMOptions((javaOptions.value ++ Seq(
        "-Dfml.ignoreInvalidMinecraftCertificates=true",
        s"-Dnet.minecraftforge.gradle.GradleStart.srg.srg-mcp=${forgeMappingSrg.value}"
      )).toVector)
      .withWorkingDirectory(minecraftHome.value),
    runClient := {
      createDirectories(minecraftHome.value)
      MinecraftLauncher.prepareModsDirectory(minecraftHome.value, forgeDevModClasspath.value, streams.value.log)

      val runner = new ForkRun(minecraftForkOptions.value)
      val forgeArgs = (Json.parse(IO.read(forgeDepsJson.value)) \ "minecraftArguments").as[String].split(" ")
      val launcherArgs = MinecraftLauncher.prepareClientLaunch(minecraftHome.value, forgeLauncherDir.value,
                                                               forgeMcVersion.value, streams.value.log)
      runner.run(
        "net.minecraft.launchwrapper.Launch",
        (fullClasspath in Forge).value.map(_.data) :+ forgeMcpBinary.value,
        forgeArgs ++ launcherArgs,
        streams.value.log
      ).get
    },
    runServer := {
      createDirectories(minecraftHome.value)
      MinecraftLauncher.prepareModsDirectory(minecraftHome.value, forgeDevModClasspath.value, streams.value.log)

      val runner = new ForkRun(minecraftForkOptions.value)
      runner.run(
        "net.minecraftforge.fml.relauncher.ServerLaunchWrapper",
        (fullClasspath in Forge).value.map(_.data) :+ forgeMcpBinary.value,
        Seq(),
        streams.value.log
      ).get
    },

    cleanKeepFiles ++= (if(cleanDownloads.value) Seq() else Seq(forgeDownloadCache.value)),
    cleanFiles ++= (if(cleanMinecraftHome.value) Seq(minecraftHome.value) else Seq()),
    // TODO: Evaluate this hack. We do this to avoid a dependency of clean on tasks that will write to target.
    clean := (Def.task { IO.delete(cleanFiles.value) } tag Tags.Clean).value
  )
}

object ForgePlugin_1_12 extends AutoPlugin {
  override def requires = BaseForgePlugin

  import BaseForgePlugin.autoImport._
  import BaseForgePluginInternalKeys._
  import LWJGLNativesPlugin.autoImport.LWJGLNativesInternalKeys._

  override def projectSettings = Seq(
    forgeVersion          := "14.23.4.2759",
    mappingChannel        := "stable_39",
    
    forgeMcMappingVersion := "1.12",
    forgeMcVersion        := "1.12.2",
    forgeScalaVersion     := "2.11.1",
    lwjglVersion          := "2.9.4-nightly-20150209",
    
    forgeExcludedOrganizations := Set(
      "org.scala-lang", "org.scala-lang.modules", "org.scala-lang.plugins", "com.typesafe.akka"
    ),
    forgeServerDepPrefixes := Seq(
      "org/bouncycastle/", "org/apache/", "com/google/", "com/mojang/authlib/", "com/mojang/util/",
      "gnu/trove/", "io/netty/", "javax/annotation/", "argo/", "it/"
    )
  )
}