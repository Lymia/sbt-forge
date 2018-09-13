package moe.lymia.forge.launcher

import java.io.File
import java.net.URL
import java.security.MessageDigest

import org.apache.commons.io.FileUtils
import moe.lymia.forge.Utils._
import play.api.libs.json.{JsObject, JsValue, Json}
import sbt._

private object MinecraftDownloader {
  private val VersionManifest = new URL("https://launchermeta.mojang.com/mc/game/version_manifest.json")

  case class DownloadSide(name: String) extends AnyVal
  val Client = DownloadSide("client")
  val Server = DownloadSide("server")

  private lazy val MinecraftDirectory = {
    val os = System.getProperty("os.name").toLowerCase
    val userHome = System.getProperty("user.home", ".")
    if(os.contains("win")) {
      val appData = System.getenv("APPDATA")
      new File(if(appData != null) appData else userHome, ".minecraft/")
    }
    else if(os.contains("mac")) new File(userHome, "Library/Application Support/minecraft")
    else if(os.contains("linux") || os.contains("unix")) new File(userHome, ".minecraft/")
    else new File(userHome, "minecraft/")
  }
}
private final class MinecraftDownloader(cacheDir: File, log: Logger) {
  private lazy val downloadsDir = cacheDir / "downloads"
  private lazy val versionsDir = cacheDir / "versions"
  private lazy val assetsDir = cacheDir / "assets"

  private def parseVersionManifest(versionManifest: String) = {
    (Json.parse(versionManifest) \ "versions").as[Seq[JsObject]].map { version =>
      ((version \ "id").as[String], new URL((version \ "url").as[String]))
    }.toMap
  }
  private def loadVersionInfo(version: String) =
    Json.parse(IO.read(cachedOperation(versionsDir / s"version_$version.json") { out =>
      val versionManifestString = downloadToString(MinecraftDownloader.VersionManifest, log)
      val versionManifest = parseVersionManifest(versionManifestString)
      val url = versionManifest.getOrElse(version, sys.error(s"No such Minecraft version $version exists!"))
      download(url, out, log)
    }))

  private def getDownloadUrl(version: String, side: MinecraftDownloader.DownloadSide) =
    (loadVersionInfo(version) \ "downloads" \ side.name \ "url").as[String]
  def downloadBinary(version: String, side: MinecraftDownloader.DownloadSide) =
    cachedOperation(downloadsDir / s"minecraft_${side.name}_$version.jar") { out =>
      download(new URL(getDownloadUrl(version, side)), out, log)
    }

  def getLibrariesFromJson(json: JsValue) =
    (json \ "libraries").as[Seq[JsObject]].map(elem => {
      val Array(org, project, version) = (elem \ "name").as[String].split(":")
      org % project % version
    })
  def getLibraries(version: String) = getLibrariesFromJson(loadVersionInfo(version))

  private final case class Asset(name: String, hash: String, length: Long)
  private def parseAssetIndex(index: File) =
    (Json.parse(IO.read(index)) \ "objects").as[Map[String, JsObject]].toSeq.map { obj =>
      Asset(obj._1, (obj._2 \ "hash").as[String], (obj._2 \ "size").as[Long])
    }
  private def downloadAssetIndex(version: String) =
    parseAssetIndex(cachedOperation(assetsDir / "indexes" / s"$version.json") { out =>
      val info = loadVersionInfo(version)
      download(new URL((info \ "assetIndex" \ "url").as[String]), out, log)
    })

  private def hashBytes(bytes: Array[Byte]) = {
    val md = MessageDigest.getInstance("SHA1")
    md.digest(bytes).map(x => "%02x".format(x & 0xff)).mkString
  }
  private def hashFile(file: File) = hashBytes(IO.readBytes(file))

  private def assetFragment(asset: Asset) =
    s"${asset.hash.substring(0, 2)}/${asset.hash}"
  private def assetDownloadUrl(asset: Asset) =
    new URL(s"http://resources.download.minecraft.net/${assetFragment(asset)}")
  private def assetPath(asset: Asset, dir: File = assetsDir) =
    dir / "objects" / assetFragment(asset)

  private def checkAsset(file: File, asset: Asset) =
    file.length == asset.length && hashFile(file) == asset.hash
  def prepareAssets(version: String, log: Logger) = {
    val index = downloadAssetIndex(version)
    val (good, bad) = index.partition { asset =>
      val file = assetPath(asset)
      file.exists && (if(!checkAsset(file, asset)) {
        log.warn(s"Asset ${asset.name} (${asset.hash}) corrupted, redownloading.")
        file.delete()
        false
      } else true)
    }
    log.info(s"Found ${good.length}/${index.length} assets.")

    val minecraftAssetDir = MinecraftDownloader.MinecraftDirectory / "assets"
    val toDownload = if(minecraftAssetDir.exists) {
      val (toCopy, remaining) = bad.partition { asset =>
        val minecraftAssetFile = assetPath(asset, minecraftAssetDir)
        minecraftAssetFile.exists && (if(!checkAsset(minecraftAssetFile, asset)) {
          log.warn(s"Asset ${asset.name} (${asset.hash}) in Minecraft directory corrupted! Ignoring.")
          false
        } else true)
      }
      for (asset <- toCopy) IO.copyFile(assetPath(asset, minecraftAssetDir), assetPath(asset))
      if (toCopy.nonEmpty) log.info(s"Copied ${toCopy.length} assets from Minecraft directory.")
      remaining
    } else bad

    if (toDownload.nonEmpty) {
      log.info (s"Downloading ${toDownload.length} assets...")
      for ((asset, i) <- toDownload.zipWithIndex) {
        val file = assetPath(asset)
        if (i % 25 == 24) log.info(s"${i + 1}/${toDownload.length} downloaded...")
        FileUtils.copyURLToFile(assetDownloadUrl(asset), file)
        if (!checkAsset(file, asset)) {
          IO.move(file, file.getParentFile / s"${file.getName}_corrupted")
          sys.error(s"Downloaded asset ${asset.name} (${asset.hash}) was corrupted!")
        }
      }
      log.info("All assets downloaded!")
    }

    Seq(
      "--assetsDir", assetsDir.toString,
      "--assetIndex", version
    )
  }
}
