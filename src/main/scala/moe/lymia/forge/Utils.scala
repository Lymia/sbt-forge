package moe.lymia.forge

import java.nio.charset.StandardCharsets
import java.nio.file._

import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}
import sbt._

import scala.collection.concurrent
import scala.collection.mutable

object Utils {
  def cachedFunction[A, B](fn: A => B) = {
    val cache = new mutable.HashMap[A, B]
    a: A => cache.getOrElse(a, {
      val v = fn(a)
      cache.put(a, v)
      v
    })
  }

  def max[T : Ordering](a: T, b: T) = implicitly[Ordering[T]].max(a, b)
  def min[T : Ordering](a: T, b: T) = implicitly[Ordering[T]].min(a, b)

  // String manipulation helpers
  val InnerClassNameRegex = """(.*)$([^$.]*)""".r.anchored

  private val classNameRegex = "([^ ]+)/([^ /]+)".r
  def splitClassName(name: String) = name match {
    case classNameRegex(owner, name) => (owner, name)
    case _ => (".", name)
  }
  def joinClassName(owner: String, name: String) =
    if (owner == ".") name
    else s"$owner/$name"

  def appendToFilename(name: String, append: String) = {
    val baseName = FilenameUtils.getBaseName(name)
    val extension = FilenameUtils.getExtension(name)
    if (extension.isEmpty) s"$baseName$append" else s"$baseName$append.$extension"
  }

  def jarFileUrl(jar: File, file: String) =
    new URL(s"jar:${jar.toURI.toURL}!/$file")

  // Extended IO tasks
  def createDirectories(file: File) =
    if (!file.exists())
      if (!file.mkdirs())
        // Another thread may have created the directory before we could
        if (!file.exists() || !file.isDirectory)
          sys.error(s"Failed to create $file!")
  def createParentDirectory(file: File) =
    createDirectories(file.getParentFile)

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

  // Caching helpers
  private def cached(cacheDirectory: File, inStyle: FileInfo.Style, outStyle: FileInfo.Style)
                    (fn: Set[File] => Set[File]) =
    FileFunction.cached(cacheDirectory, inStyle, outStyle)(fn)
  def trackDependencies(cacheDirectory: File, deps: Set[File],
                        inStyle: FileInfo.Style = FilesInfo.lastModified,
                        outStyle: FileInfo.Style = FilesInfo.exists)(fn: => File) = {
    val cache = cached(cacheDirectory, inStyle, outStyle) { _ => Set(fn) }
    cache(deps).head
  }
  def cachedTransform(cacheDirectory: File, input: File, output: File,
                      inStyle: FileInfo.Style = FilesInfo.lastModified,
                      outStyle: FileInfo.Style = FilesInfo.exists)(fn: (File, File) => Unit) = {
    val cache = cached(cacheDirectory, inStyle, outStyle){ in =>
      fn(in.head, output)
      Set(output)
    }
    cache(Set(input))
    output
  }
  def cachedGeneration(cacheDirectory: File, target: File, data: String) = {
    val tempTarget = cacheDirectory / s"temp_${target.getName}"
    IO.write(tempTarget, data)
    cachedTransform(cacheDirectory, tempTarget, target, inStyle = FilesInfo.hash)((in, out) =>
      IO.copyFile(in, out))
  }

  // There will be so few locks that this should be OK to leak. (We only use this for downloads currently)
  private val LockSets = new concurrent.TrieMap[File, Object]
  def cachedOperation[T](outFile: File)(task: File => T) =
    LockSets.getOrElseUpdate(outFile.getCanonicalFile, new Object) synchronized {
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
}
