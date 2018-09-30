package moe.lymia.forge.mapper

import java.util.jar.JarFile

import moe.lymia.forge.asm._
import moe.lymia.forge.Utils._
import sbt._

import scala.collection.mutable
import scala.collection.JavaConverters._

private object classpath {
  private val classFileRegex = "([^.]+)\\.class".r
  private def findClassesInDirectory(path: String, file: File): Seq[(String, URL)] =
    if (file.isDirectory) file.listFiles.flatMap { f =>
        val npath = if(path == "") f.getName else s"$path/${f.getName}"
        findClassesInDirectory(npath, f)
    } else path match {
      case classFileRegex(name) => Seq((name, file.toURI.toURL))
      case _ => Nil
    }
  private def findClassesInJar(file: File) =
    new JarFile(file).entries().asScala.map(_.getName).collect {
      case fileName @ classFileRegex(className) =>
        (className, jarFileUrl(file, fileName))
    }
  private def findClassNamesInJar(file: File) =
    findClassesInJar(file).map(_._1)
  private def findClasses(file: File) =
    if(file.isDirectory) findClassesInDirectory("", file)
    else                 findClassesInJar      (file)

  private lazy val SbtClasses =
    System.getProperty("java.class.path").split(System.getProperty("path.separator"))
      .map(_.trim).filter(!_.isEmpty)
      .flatMap(x => findClassNamesInJar(new File(x)))
      .toSet
  private def loadSystemClass(name: String) =
    if(SbtClasses.contains(name)) None
    else Option(ClassLoader.getSystemResource(s"$name.class"))

  private def loadClassByURL(url: URL) =
    new ClassNodeWrapper(readClassNode(IO.readBytes(url.openStream())), noCopy = true)

  // The class responsible for loading class data from the classpath.
  final class ClasspathSearcher(var targetJar: JarData, classpath: Seq[File], log: Logger) {
    private val classSourcesMap  = new mutable.HashMap[String, URL]
    private val classLocationMap = new mutable.HashMap[String, String]

    private val resolveRawClasspath = cachedFunction { name: String =>
      loadSystemClass(name).map(x => ("<system classloader>", loadClassByURL(x)))
        .orElse(classSourcesMap.get(name).map(x => (classLocationMap(name), loadClassByURL(x))))
    }
    private def resolveRawAll(name: String) =
      targetJar.getClass(name).map(x => ("<target jar>", x)).orElse(resolveRawClasspath(name))
    private def resolveRaw(name: String, source: String) =
      resolveRawAll(name).getOrElse(sys.error(
        s"Validation error: Reference to undefined class $name" +
        (if (source == null) "" else s" from $source in ${resolveRawAll(source).fold("<unknown source>")(_._1)}")))

    def classLocation(name: String, source: String = null) =
      resolveRaw(name, source)._1
    def loadSymbols(name: String, source: String = null) =
      resolveRaw(name, source)._2
    def targetClasses = targetJar.classes.keySet

    for(file <- classpath) {
      log.debug(s"Indexing classes in $file")
      val classes = findClasses(file).toSeq
      for((name, url) <- classes)
        if(targetJar.classes.contains(name))
          log.warn(s"$file defines class $name already defined in target jar!")
        else if(classLocationMap.contains(name))
          log.warn(s"$file defines class $name already defined in ${classLocation(name)}!")
        else {
          classSourcesMap.put(name, url)
          classLocationMap.put(name, file.getName)
        }
    }
  }
}
