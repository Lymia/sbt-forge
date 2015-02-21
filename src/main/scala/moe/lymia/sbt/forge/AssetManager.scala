package moe.lymia.sbt.forge

import sbt._
import play.api.libs.json._

import java.io._
import java.net.URL
import java.security._

object AssetManager {
  case class Asset(name: String, hash: String, length: Long)
  def loadIndex(index: File) = {
    (Json.parse(IO.read(index)) \ "objects").as[Map[String, JsObject]].toSeq.map { obj =>
      Asset(obj._1, (obj._2 \ "hash").as[String], (obj._2 \ "size").as[Long])
    }
  }

  def hashBytes(bytes: Array[Byte]) = {
    val md = MessageDigest.getInstance("SHA1")
    md.digest(bytes).map(x => "%02x".format(x & 0xff)).reduce(_ + _)
  }
  def hashFile(file: File) = hashBytes(IO.readBytes(file))

  def assetFragment(asset: Asset) = asset.hash.substring(0, 2) + "/" + asset.hash
  def assetDownloadUrl(asset: Asset) = new URL("http://resources.download.minecraft.net/"+assetFragment(asset))
  def assetPath(file: File, asset: Asset) = file / "objects" / assetFragment(asset)

  def checkAsset(file: File, asset: Asset) = file.length == asset.length && hashFile(file) == asset.hash
  def prepareAssets(assets: File, indexFile: File, log: Logger) {
    val index = loadIndex(indexFile)
    val (good, bad) = index.partition { asset =>
      val file = assetPath(assets, asset)
      file.exists && (if(!checkAsset(file, asset)) {
        log.warn("Asset "+asset.name+" ("+asset.hash+") corrupted, redownloading.")
        file.delete()
        false
      } else true)
    }
    log.info("Found "+good.length+"/"+index.length+" assets.")

    val toDownload = if((minecraftDirectory / "assets").exists) {
      val (toCopy, remaining) = bad.partition { asset =>
        val file = assetPath(minecraftDirectory / "assets", asset)
        file.exists && (if(!checkAsset(file, asset)) {
          log.warn("Asset "+asset.name+" ("+asset.hash+") in Minecraft directory corrupted! Ignoring.")
          false
        } else true)
      }
      for(asset <- toCopy) IO.copyFile(assetPath(minecraftDirectory / "assets", asset), assetPath(assets, asset))
      if(toCopy.length != 0) log.info("Copied "+toCopy.length+" assets from Minecraft directory.")
      remaining
    } else bad

    if(toDownload.length != 0) {
      log.info("Downloading "+toDownload.length+" assets...")
      for((asset, i) <- toDownload.zipWithIndex) {
        val file = assetPath(assets, asset)
        if((i+1)%25 == 0) log.info((i+1)+"/"+toDownload.length+" downloaded...")
        IO.download(assetDownloadUrl(asset), file)
        if(!checkAsset(file, asset)) {
          file.delete()
          sys.error("Downloaded asset "+asset.name+" ("+asset.hash+") was corrupted!")
        }
      }
      log.info("All assets downloaded!")
    }
  }
}
