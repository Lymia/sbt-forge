package moe.lymia.forge.launcher

import moe.lymia.forge.Utils._
import play.api.libs.json.Json
import sbt._
import sbt.Keys._

object MinecraftLauncher {
  def downloadClient(cacheDir: File, version: String, log: Logger) =
    new MinecraftDownloader(cacheDir, log).downloadBinary(version, MinecraftDownloader.Client)
  def downloadServer(cacheDir: File, version: String, log: Logger) =
    new MinecraftDownloader(cacheDir, log).downloadBinary(version, MinecraftDownloader.Server)

  def getDependencies(cacheDir: File, version: String, extraJsonFiles: Seq[File], log: Logger) = {
    val downloader = new MinecraftDownloader(cacheDir, log)
    downloader.getLibraries(version) ++
      extraJsonFiles.flatMap(file => downloader.getLibrariesFromJson(Json.parse(IO.read(file))))
  }

  def login(cacheDir: File, log: Logger) = new AuthManager(cacheDir, log).login()
  def logout(cacheDir: File, log: Logger) = new AuthManager(cacheDir, log).logout()

  def prepareModsDirectory(runDir: File, modFiles: Classpath, log: Logger) = {
    val modsDir = runDir / "mods"

    log.info("Preparing mods directory...")
    if (modsDir.exists()) IO.delete(modsDir)
    createDirectories(modsDir)
    for (modFile <- modFiles) {
      val modFileName = findUnusedFile(modFile.data.getName, x => (modsDir / x).exists())
      val modTarget = modsDir / modFileName
      log.info(s"Linking $modFileName to $modTarget.")
      ln(modFile.data, modTarget)
    }
  }

  def prepareClientLaunch(runDir: File, cacheDir: File, version: String, log: Logger) = {
    val authentication = new AuthManager(cacheDir, log)
    val downloader = new MinecraftDownloader(cacheDir, log)

    downloader.prepareAssets(version, log) ++ authentication.loginWithToken() ++ Seq(
      "--gameDir", runDir.toString
    )
  }
}
