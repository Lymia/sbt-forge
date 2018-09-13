package moe.lymia.forge

import java.nio.charset.StandardCharsets
import java.nio.file._

import org.apache.commons.io.{FileUtils, IOUtils}
import sbt._

object Utils {
  def createDirectories(file: File) =
    if (!file.exists())
      if (!file.mkdirs())
        // Another thread may have created the directory before we could
        if (!file.exists() || !file.isDirectory)
          sys.error(s"Failed to create $file!")
  def createParentDirectory(file: File) =
    createDirectories(file.getParentFile)

  // TODO: Add a file lock to this.
  def cachedFile[T](outFile: File)(task: File => T) = {
    if(!outFile.exists) try {
      createParentDirectory(outFile)
      task(outFile)
    } catch {
      case t: Throwable =>
        if(outFile.exists) outFile.delete()
        throw t
    }
    outFile
  }

  def download(url: URL, outFile: File, logger: Logger = null) = {
    if (logger != null) logger.info(s"Downloading $url to $outFile...")
    FileUtils.copyURLToFile(url, outFile)
  }
  def downloadToString(url: URL, logger: Logger = null) = {
    if (logger != null) logger.info(s"Downloading $url to string...")
    IOUtils.toString(url, StandardCharsets.UTF_8)
  }

  def ln(source: File, target: File) =
    Files.createSymbolicLink(Paths.get(target.getCanonicalPath),
                             Paths.get(source.getCanonicalPath))
}
