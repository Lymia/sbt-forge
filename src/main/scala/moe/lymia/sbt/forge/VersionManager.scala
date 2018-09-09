package moe.lymia.sbt.forge

import sbt._
import play.api.libs.json._

import java.io._
import java.net.URL
import java.security._

object VersionManager {
  private def parseVersionManifest(versionManifest: File) = {
    (Json.parse(IO.read(versionManifest)) \ "versions").as[Seq[JsObject]].map { version =>
      ((version \ "id").as[String], new URL((version \ "url").as[String]))
    }.toMap
  }
  private def loadVersionInfo(versionCache: File, version: String) = {
    val cacheFile = versionCache / s"version_$version.json"
    if (!cacheFile.exists()) {
      val versionManifestFile = versionCache / "version_manifest.json"
      IO.download(new URL("https://launchermeta.mojang.com/mc/game/version_manifest.json"), versionManifestFile)
      val versionManifest = parseVersionManifest(versionManifestFile)
      val url = versionManifest.getOrElse(version, sys.error(s"No such Minecraft version $version exists!"))
      IO.download(url, cacheFile)
    }
    Json.parse(IO.read(cacheFile))
  }


  def getClientDownloadUrl(versionCache: File, version: String) =
    (loadVersionInfo(versionCache, version) \ "downloads" \ "client" \ "url").as[String]
  def getServerDownloadUrl(versionCache: File, version: String) =
    (loadVersionInfo(versionCache, version) \ "downloads" \ "server" \ "url").as[String]

  def getLibrariesFromJson(json: JsValue) =
    (json \ "libraries").as[Seq[JsObject]].map(elem => {
      val Array(org, project, version) = (elem \ "name").as[String].split(":")
      org % project % version
    })
  def getLibraries(versionCache: File, version: String) =
    getLibrariesFromJson(loadVersionInfo(versionCache, version))

  private case class Asset(name: String, hash: String, length: Long)
  private def parseAssetIndex(index: File) =
    (Json.parse(IO.read(index)) \ "objects").as[Map[String, JsObject]].toSeq.map { obj =>
      Asset(obj._1, (obj._2 \ "hash").as[String], (obj._2 \ "size").as[Long])
    }
  private def loadAssetIndex(versionCache: File, version: String) = {
    val assetIndexFile = versionCache / s"asset_index_$version.json"
    if (!assetIndexFile.exists()) {
      val info = loadVersionInfo(versionCache, version)
      IO.download(new URL((info \ "assetIndex" \ "url").as[String]), assetIndexFile)
    }
    parseAssetIndex(assetIndexFile)
  }

  private def hashBytes(bytes: Array[Byte]) = {
    val md = MessageDigest.getInstance("SHA1")
    md.digest(bytes).map(x => "%02x".format(x & 0xff)).mkString
  }
  private def hashFile(file: File) = hashBytes(IO.readBytes(file))

  private def assetFragment(asset: Asset) =
    s"${asset.hash.substring(0, 2)}/${asset.hash}"
  private def assetDownloadUrl(asset: Asset) =
    new URL(s"http://resources.download.minecraft.net/${assetFragment(asset)}")
  private def assetPath(file: File, asset: Asset) =
    file / "objects" / assetFragment(asset)

  private def checkAsset(file: File, asset: Asset) =
    file.length == asset.length && hashFile(file) == asset.hash
  def prepareAssets(assets: File, versionCache: File, version: String, log: Logger) {
    val index = loadAssetIndex(versionCache, version)
    val (good, bad) = index.partition { asset =>
      val file = assetPath(assets, asset)
      file.exists && (if(!checkAsset(file, asset)) {
        log.warn(s"Asset ${asset.name} (${asset.hash}) corrupted, redownloading.")
        file.delete()
        false
      } else true)
    }
    log.info(s"Found ${good.length}/${index.length} assets.")

    val toDownload = if((minecraftDirectory / "assets").exists) {
      val (toCopy, remaining) = bad.partition { asset =>
        val file = assetPath(minecraftDirectory / "assets", asset)
        file.exists && (if(!checkAsset(file, asset)) {
          log.warn(s"Asset ${asset.name} (${asset.hash}) in Minecraft directory corrupted! Ignoring.")
          false
        } else true)
      }
      for (asset <- toCopy) IO.copyFile(assetPath(minecraftDirectory / "assets", asset), assetPath(assets, asset))
      if (toCopy.nonEmpty) log.info(s"Copied ${toCopy.length} assets from Minecraft directory.")
      remaining
    } else bad

    if (toDownload.nonEmpty) {
      log.info (s"Downloading ${toDownload.length} assets...")
      for ((asset, i) <- toDownload.zipWithIndex) {
        val file = assetPath(assets, asset)
        if (i % 25 == 1) log.info(s"${i + 1}/${toDownload.length} downloaded...")
        IO.download(assetDownloadUrl(asset), file)
        if (!checkAsset(file, asset)) {
          file.delete()
          sys.error(s"Downloaded asset ${asset.name} (${asset.hash}) was corrupted!")
        }
      }
      log.info("All assets downloaded!")
    }
  }
}