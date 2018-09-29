package moe.lymia.forge.build

import java.io.{ByteArrayOutputStream, FileOutputStream, PrintStream}
import java.util.jar.{Attributes, Manifest}
import java.util.zip.ZipFile

import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import org.apache.commons.io.FilenameUtils
import org.objectweb.asm.commons.{ClassRemapper, Remapper}
import sbt.{File, IO, Logger, ModuleID}

import scala.collection.JavaConverters._

case class ShadeMapping(classMapping: Map[String, String]) extends Remapper {
  override def map(typeName: String): String = classMapping.getOrElse(typeName, typeName)

  def mapJar(jar: JarData) = jar.mapWithVisitor(cv => new ClassRemapper(cv, this))

  def write(target: File) {
    val out = new PrintStream(new FileOutputStream(target))
    try {
      for ((from, to) <- classMapping) out.println(s"shade $from -> $to")
    } finally {
      out.close()
    }
  }
}
object ShadeMapping {
  def read(target: File) = ShadeMapping(IO.readLines(target).map(_.split(" ")).map {
    case Array("shade", from, "->", to) => from -> to
    case line => sys.error(s"Failed to parse shade mapping line: ${line.mkString(" ")}")
  }.toMap)
}

object DepShader {
  private val ContainedDeps = new Attributes.Name("ContainedDeps")
  private val MavenArtifact = new Attributes.Name("Maven-Artifact")
  private val Timestamp     = new Attributes.Name("Timestamp")

  private def addExtractedDep(target: JarData, file: File, moduleId: Option[ModuleID]) = {
    val prefix = findUnusedFile(s"META-INF/libraries/${file.getName}",
                                x => target.resources.contains(x) || target.resources.contains(s"$x.meta"))
    val fileName = FilenameUtils.getName(prefix)
    target.resources.put(prefix, IO.readBytes(file))
    for (module <- moduleId) {
      val meta = new Manifest()
      val zipFile = new ZipFile(file)
      val timestamp = try {
        zipFile.entries().asScala.map(_.getLastModifiedTime.toMillis).max
      } finally {
        zipFile.close()
      }
      meta.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      meta.getMainAttributes.put(MavenArtifact, module.toString())
      meta.getMainAttributes.put(Timestamp, timestamp.toString)
      val out = new ByteArrayOutputStream()
      meta.write(out)
      target.resources.put(s"$prefix.meta", out.toByteArray)
    }

    val currentDeps = target.manifest.getMainAttributes.asScala.getOrElse(ContainedDeps, "").asInstanceOf[String]
    val deps = if (currentDeps.isEmpty) fileName else s"$currentDeps $fileName"
    target.manifest.getMainAttributes.put(ContainedDeps, deps)
  }
  def shadeDeps(target: JarData,
                depList: Seq[(JarData, Option[String])],
                extractedDeps: Seq[(File, Option[ModuleID])],
                log: Logger = null) = {
    val shadeMapping =
      ShadeMapping((for ((dep, shadePrefix) <- depList;
                          name <- dep.classes.keySet;
                          shadePrefix <- shadePrefix) yield name -> s"$shadePrefix/$name").toMap)
    val merged = JarData.mergeAll(depList.map(_._1) :+ target, log)
    merged.manifest = target.manifest
    val mapped = shadeMapping.mapJar(merged)
    for ((file, moduleId) <- extractedDeps) addExtractedDep(mapped, file, moduleId)
    mapped
  }
}
