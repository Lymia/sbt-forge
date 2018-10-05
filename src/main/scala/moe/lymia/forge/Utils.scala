package moe.lymia.forge

import java.nio.charset.StandardCharsets
import java.nio.file._

import com.google.common.collect.MapMaker
import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}
import sbt._

import scala.collection.JavaConverters._
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

  def cleanModuleID(id: ModuleID) = id.organization % id.name % id.revision

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
    val baseName = FilenameUtils.removeExtension(name)
    val extension = FilenameUtils.getExtension(name)
    if (extension.isEmpty) s"$baseName$append" else s"$baseName$append.$extension"
  }
  def findUnusedFile(file: String, exists: String => Boolean) = {
    var result = file
    var current = 2
    while (exists(result)) {
      result = appendToFilename(file, s"_$current")
      current += 1
    }
    result
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
  private val lockSets = new MapMaker().weakValues().makeMap[File, Object]().asScala

  private def cached(cacheDirectory: File, extra: String, inStyle: FileInfo.Style, outStyle: FileInfo.Style)
                    (fn: Set[File] => Set[File]) =
    lockSets.getOrElseUpdate(cacheDirectory.getCanonicalFile, new Object) synchronized {
      val extraFile = cacheDirectory / "extra"
      val flagFile = cacheDirectory / "extra_changed_flag"
      IO.write(extraFile, extra)
      deps: Set[File] => {
        FileFunction.cached(cacheDirectory / "extra_cache", FileInfo.hash, outStyle) { _ =>
          // We only touch the flag file when extra changes hash. So any inStyle works in the actual call.
          IO.write(flagFile, Hash.toHex(Hash(extra)))
          Set(flagFile)
        }(Set(extraFile))
        FileFunction.cached(cacheDirectory / "main_cache", inStyle, outStyle)(x => fn(x - flagFile))(deps + flagFile)
      }
    }
  def trackDependencies(cacheDirectory: File, deps: Set[File],
                        inStyle: FileInfo.Style = FilesInfo.lastModified,
                        outStyle: FileInfo.Style = FilesInfo.exists,
                        extra: String = "")(fn: => File) = {
    val cache = cached(cacheDirectory, extra, inStyle, outStyle) { _ => Set(fn) }
    cache(deps).head
  }
  def cachedTransform(cacheDirectory: File, input: File, output: File,
                      inStyle: FileInfo.Style = FilesInfo.lastModified,
                      outStyle: FileInfo.Style = FilesInfo.exists,
                      extra: String = "")(fn: (File, File) => Unit) = {
    val cache = cached(cacheDirectory, extra, inStyle, outStyle){ in =>
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

  def cachedOperation[T](outFile: File)(task: File => T) =
    lockSets.getOrElseUpdate(outFile.getCanonicalFile, new Object) synchronized {
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
