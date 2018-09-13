package moe.lymia.forge

import java.io._
import java.util.jar._

import moe.lymia.forge.asm._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable

object classpath {
  // Classpath code
  def isSystemClass(name: String) = 
    try {
      ClassLoader.getSystemClassLoader.loadClass(name.replace("/", "."))
      true
    } catch {
      case _: Throwable => false
    }

  val classFileRegex = "([^.]+)\\.class".r
  private def findClassesInDirectory(path: String, file: File): Seq[(String, () => InputStream)] = 
    if(file.isDirectory) {
      file.listFiles.flatMap { f =>
        val npath = if(path == "") f.getName else s"$path/${f.getName}"
        findClassesInDirectory(npath, f)
      }
    } else path match {
      case classFileRegex(name) => Seq((name, () => new FileInputStream(file)))
      case _ => Seq()
    }
  private def findClassesInJar(file: File) = {
    val jarFile = new JarFile(file)
    (for(entry <- jarFile.entries().asScala) yield entry.getName match {
      case classFileRegex(name) =>
        Some((name, () => new URL(s"jar:${file.toURI.toURL}!/${entry.getName}").openStream()))
      case _ => None
    }).flatten
  }
  private def findClasses(file: File) =
    if(file.isDirectory) findClassesInDirectory("", file)
    else                 findClassesInJar      (file)
  class ClasspathSearcher(targetJar: JarData, classpath: Seq[File], log: Logger) {
    private val classSources   = new mutable.HashMap[String, () => InputStream]
    private val classLocation  = new mutable.HashMap[String, File]
    private val sourceContents = new mutable.HashMap[File, Seq[String]]

    val resolve = cacheFunction { name: String => targetJar.classes.get(name) orElse
      (classSources.get(name) match { 
        case Some(x) =>
          Some(new ClassNodeWrapper(readClassNode(x()), noCopy = true))
        case None =>
          if(!classExists(name)) log.warn(s"Attempt to resolve non-existant class $name!")
          None
      })
    }

    def sourceContents(file: File): Option[Seq[String]] = sourceContents.get(file)
    def classExists(name: String) = targetJar.classes.contains(name) || classLocation.contains(name) || isSystemClass(name)
    def classLocationStr(name: String): String =
      targetJar.classes.get(name).map(_ => "<target jar>") orElse
      classLocation.get(name).map(_.getName) getOrElse
      (if(isSystemClass(name)) "<system classloader>" else "<unknown>")

    for(file <- classpath) {
      log.debug(s"Indexing classes in $file")
      val classes = findClasses(file).toSeq
      sourceContents.put(file, classes.map(_._1).toSeq)
      for((name, loader) <- classes)
        if(targetJar.classes.contains(name)) log.warn(s"$file defines class $name already defined in target jar!")
        else if(classLocation.contains(name)) log.warn(s"$file defines class $name already defined in ${classLocationStr(name)}!")
        else {
          classSources.put(name, loader)
          classLocation.put(name, file)
        } 
    }
  }
}