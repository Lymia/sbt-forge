package moe.lymia.forge.build

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.jar.{Attributes, Manifest}
import java.util.zip.ZipFile

import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import org.apache.commons.io.FilenameUtils
import org.objectweb.asm.commons.{ClassRemapper, Remapper}
import play.api.libs.json._
import sbt._

import scala.collection.JavaConverters._

final case class ShadeMapping(classMapping: Map[String, String]) extends Remapper {
  override def map(typeName: String): String = classMapping.getOrElse(typeName, typeName)

  def mapJar(jar: JarData) = jar.mapWithVisitor(cv => new ClassRemapper(cv, this))
}

object DepShader {
  private implicit val shadeMappingFormat = Json.format[ShadeMapping]

  private val ContainedDeps = new Attributes.Name("ContainedDeps")
  private val MavenArtifact = new Attributes.Name("Maven-Artifact")
  private val Timestamp     = new Attributes.Name("Timestamp")

  private val ShadeMappingRes = "META-INF/sbt-forge/shade-mapping.json"

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
  def generateDepsJar(depList: Seq[(File, Option[String])], extractedDeps: Seq[(File, Option[ModuleID])],
                      log: Logger = null) = {
    val depFiles = depList.map(x => (JarData.load(x._1).stripSignatures, x._2))
    val shadeMapping =
      ShadeMapping((for ((dep, shadePrefix) <- depFiles;
                         name <- dep.classes.keySet;
                         shadePrefix <- shadePrefix) yield name -> s"$shadePrefix/$name").toMap)
    val merged = JarData.mergeAll(depFiles.map(_._1), log)
    val mapped = shadeMapping.mapJar(merged)
    mapped.manifest = new Manifest()
    mapped.manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    for ((file, moduleId) <- extractedDeps) addExtractedDep(mapped, file, moduleId)
    mapped.resources.put(ShadeMappingRes, Json.toJson(shadeMapping).toString().getBytes(StandardCharsets.UTF_8))
    mapped
  }
  def addDepsToJar(target: File, dep: File) = {
    val targetJar = JarData.load(target)
    val depJar = JarData.load(dep)
    val data = depJar.resources.getOrElse(ShadeMappingRes, sys.error(s"$ShadeMappingRes not found in shaded deps."))
    val shadeMapping = Json.parse(data).as[ShadeMapping]
    val mappedTarget = shadeMapping.mapJar(targetJar)
    depJar.resources.remove(ShadeMappingRes)
    depJar.mergeWith(mappedTarget)
  }
}
