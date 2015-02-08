package moe.lymia.sbt

import sbt._
import Keys._

import LWJGLSupport._

import java.net.URL
import java.io.File

import language._

object ForgePlugin extends Plugin {
  object forge { 
    // User setting keys
    val mcVersion   = SettingKey[String]("forge-minecraft-version")
    val version     = SettingKey[String]("forge-version")

    val fullVersion = SettingKey[String]("forge-full-version")
    val cacheDir    = SettingKey[File]("forge-cache-dir",
      "Directory used to store files used by sbt-forge")

    val cleanCache  = SettingKey[Boolean]("forge-clean-cache",
      "If set, sbt clean will erase the cache directory and force a redownload.")

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
    val downloadClientJar      = TaskKey[File]("forge-download-client-jar",
      "Downloads the Minecraft client binary")
    val downloadServerJar      = TaskKey[File]("forge-download-server-jar",
      "Downloads the Minecraft server binary")
    val downloadUniversalJar   = TaskKey[File]("forge-download-universal-jar", 
      "Downloads the Forge binary")
    val downloadUserdevArchive = TaskKey[File]("forge-download-userdev-archive", 
      "Downloads the userdev archive")

    // Patch and merge client .jars
    val extractPatchData       = TaskKey[File]("forge-extract-patch-data",
      "Extracts Forge's patches from the universal binary")
    val patchServerJar         = TaskKey[File]("forge-patch-server-jar",
      "Applies Forge's patches to the Minecraft server binary")
    val patchClientJar         = TaskKey[File]("forge-patch-client-jar",
      "Applies Forge's patches to the Minecraft client binary")
    val mergeJars              = TaskKey[File]("forge-merge-jars",
      "Merges the Minecraft client and server binaries.")
  }

  object forgeHelpers {
    def defaultDownloadUrl(section: String) =
      forge.fullVersion map { ver =>
        "http://files.minecraftforge.net/maven/net/minecraftforge/forge/" +
          ver + "/forge-" + ver + "-" + section + ".jar"
      }

    def copyUrl(target: File, source: String, log: Logger) = {
      if(!target.exists) {
        if(!target.getParentFile.exists)
          if(!target.getParentFile.mkdirs())
            sys.error("Failed to create parent directory of "+target.getCanonicalPath)
        log.info("Copying "+source+" to "+target.getCanonicalPath+"...")
        new URL(source) #> target !!;
        log.info("Done copying "+source+".")
      } else {
        log.info(target.getCanonicalPath+" already exists, skipping.")
      }
      target
    }
    def copyUrlTask[T](urlSource: TaskKey[T], outputName: String, versionSource: SettingKey[String],
                       urlFilter: T => String = (x: Any) => x.toString, extension: String = ".jar") =
      (forge.cacheDir, urlSource, versionSource, streams) map
        ((dir, url, ver, streams) => copyUrl(dir / (outputName+"-"+ver+extension), urlFilter(url), streams.log))

    def jarFileUrl(jar: File, file: String) =
      "jar:"+jar.toURI.toURL+"!/"+file

    def patchJarTask(input: TaskKey[File], outputName: String, patchSection: String) =
      (forge.cacheDir, input, forge.universalDownloadUrl, forge.fullVersion, forge.extractPatchData, streams) map 
        { (dir, in, universal, ver, patchDataFile, streams) =>
          val log = streams.log
          val outputFile = dir / (outputName+"-"+ver+".jar")
          if(!outputFile.exists) {
            val patchSet = ForgePatch.readPatchSet(patchDataFile, patchSection)
            ForgePatch.patchJar(in, outputFile, patchSet, log)
          }
          outputFile
        }
  }
  import forgeHelpers._

  lazy val forgeSettings: Seq[Setting[_]] = lwjglSettings ++ Seq(
    forge.mcVersion   := "1.7.10",
    forge.version     := "10.13.2.1230",
    forge.fullVersion <<= (forge.mcVersion, forge.version) apply (_ + "-" + _),

    forge.cleanCache  := true,
    forge.cacheDir    <<= target apply (_ / "sbt-forge-cache"),

    forge.clientDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/"+ver+".jar"),
    forge.serverDownloadUrl    <<= forge.mcVersion map (ver => 
      "https://s3.amazonaws.com/Minecraft.Download/versions/"+ver+"/minecraft_server."+ver+".jar"),
    forge.universalDownloadUrl <<= defaultDownloadUrl("universal"),
    forge.userdevDownloadUrl   <<= defaultDownloadUrl("userdev"),

    forge.downloadClientJar      <<= copyUrlTask(forge.clientDownloadUrl   , "minecraft_client", forge.mcVersion),
    forge.downloadServerJar      <<= copyUrlTask(forge.serverDownloadUrl   , "minecraft_server", forge.mcVersion),
    forge.downloadUniversalJar   <<= copyUrlTask(forge.universalDownloadUrl, "forge_universal" , forge.fullVersion),
    forge.downloadUserdevArchive <<= copyUrlTask(forge.userdevDownloadUrl  , "forge_userdev"   , forge.fullVersion),

    forge.extractPatchData       <<= copyUrlTask(forge.downloadUniversalJar, "binpatches", forge.fullVersion,
                                                 urlFilter = (x: File) => jarFileUrl(x, "binpatches.pack.lzma"), extension = ".pack.lzma"),
    forge.patchClientJar         <<= patchJarTask(forge.downloadClientJar, "minecraft_client_patched", "client"),
    forge.patchServerJar         <<= patchJarTask(forge.downloadServerJar, "minecraft_server_patched", "server"),
    forge.mergeJars              <<= (forge.cacheDir, forge.patchClientJar, forge.patchServerJar, forge.fullVersion, streams) map 
      { (dir, client, server, ver, streams) =>
        val log = streams.log
        val outFile = dir / ("minecraft_merged-"+ver+".jar")
        if(!outFile.exists) {
          val clientZip = asmstuff.Util.openZip(client.toPath)
          val serverZip = asmstuff.Util.openZip(server.toPath)
          val outZip = asmstuff.Util.openZip(outFile.toPath, true)
          asmstuff.Merger.mergeJars(clientZip.getPath("/"), serverZip.getPath("/"), outZip.getPath("/"))
          clientZip.close()
          serverZip.close()
          outZip.close()
        }
        outFile
      },

    cleanFiles <++= (forge.cacheDir, forge.cleanCache) map { (dir, clean) => if(clean) Seq(dir) else Seq() }
  )
}
