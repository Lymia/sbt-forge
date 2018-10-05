package moe.lymia.forge.build

import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.util.jar.{Attributes, Manifest}
import java.util.zip.ZipFile

import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import org.apache.commons.io.FilenameUtils
import org.objectweb.asm.commons.{ClassRemapper, Remapper}
import play.api.libs.json._
import sbt.Keys._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable

private final case class ShadeMapping(classMapping: Map[String, String]) extends Remapper {
  override def map(typeName: String): String = classMapping.getOrElse(typeName, typeName)
  def mapJar(jar: JarData) = jar.mapWithVisitor(cv => new ClassRemapper(cv, this))
}

private sealed trait ShadeAction
private object ShadeAction {
  case object NotShaded extends ShadeAction
  case object Extract extends ShadeAction
  case object Shade extends ShadeAction
  case object Provided extends ShadeAction
}

private object ShadeFlag extends Enumeration {
  type ShadeFlag = Value
  type ShadeFlags = ValueSet
  val ShadeFlags = ValueSet

  val Forge, Extract, Shade, ShadeDeps, Optional, Provided = Value
  val UsedShadeFlags = Set(Forge, Extract, Shade, ShadeDeps, Optional)

  private val ForgeConfigurations = Set("forge", "optional", "provided")
  private val FlagConfigurations = Set("shade", "shadeDeps", "extract")
  private def parseConfigurationString(s: String) =
    s.split(";").map(_.split("->")).collect {
      case Array(x) if FlagConfigurations.contains(x) || ForgeConfigurations.contains(x) => x
      case Array(a, b) if FlagConfigurations.contains(a) || FlagConfigurations.contains(b) =>
        sys.error(s"Invalid configuration $a->$b: [${FlagConfigurations.mkString(", ")}] cannot be used in a->b form.")
      case x if x.length > 2 =>
        sys.error(s"Invalid configuration ${x.mkString("->")}")
    }.toSet
  private def parseConfiguration(m: ModuleID) =
    m.configurations.fold(Set.empty[String])(parseConfigurationString)

  implicit class ShadeFlagsExt(obj: ShadeFlags.type) {
    def forModule(moduleId: ModuleID) =
      ShadeFlags(parseConfiguration(moduleId).map {
        case "forge" => Forge
        case "extract" => Extract
        case "shade" => Shade
        case "shadeDeps" => ShadeDeps
        case "optional" => Optional
        case "provided" => Provided
      }.toSeq: _*)
  }
}
import ShadeFlag._

final case class ShadeClasspaths(modClasspath: Classpath, shade: Classpath, extract: Classpath,
                                 private val rawShadePrefix: String) {
  lazy val shadePrefix = rawShadePrefix.replace('.', '/')
  lazy val trackFiles = (modClasspath ++ shade ++ extract).map(_.data).toSet
  lazy val trackExtra = Seq(modClasspath, shade, extract).map(_.map(_.data).sorted.mkString(" ")).mkString("\n")
}
final class ShadeInfo(explicitModules: Seq[ModuleID], updateReport: UpdateReport,
                      classpath: Classpath, shadePrefix: String) {
  private val annotatedClasspath = {
    val reverseDeps = {
      val compile = updateReport.configurations.find(_.configuration.name == "compile")
        .getOrElse(sys.error("compile configuration not found in update report"))
      compile.modules
        .map(x => cleanModuleID(x.module) -> x.callers.map(x => cleanModuleID(x.caller)).toSet).toMap
    }
    val deps = {
      val map = new mutable.HashMap[ModuleID, mutable.Set[ModuleID]] with mutable.MultiMap[ModuleID, ModuleID]
      for ((callee, callers) <- reverseDeps;
           caller <- callers) map.addBinding(caller, callee)
      map
    }

    val chosenActions = new mutable.HashMap[ModuleID, ShadeAction]
    def addAction(id: ModuleID, action: ShadeAction) =
      (action, chosenActions.getOrElse(cleanModuleID(id), ShadeAction.NotShaded)) match {
        case (_, ShadeAction.NotShaded) |
             (ShadeAction.Shade, ShadeAction.Extract) |
             (ShadeAction.Provided, _) => chosenActions.put(cleanModuleID(id), action)
        case _ =>
      }

    for (rawModule <- explicitModules) {
      val module = cleanModuleID(rawModule)
      val configurations = ShadeFlags.forModule(rawModule)
      if (configurations.contains(ShadeFlag.Forge) ||
          configurations.contains(ShadeFlag.Provided)) addAction(module, ShadeAction.Provided)
      else {
        // Mark sure anything on the explicit classpath or is a dependency of one
        addAction(module, ShadeAction.NotShaded)
        for (depList <- deps.get(module); dep <- depList) addAction(dep, ShadeAction.NotShaded)

        // Calculate any further actions that have to be taken with a dependency
        if (configurations.contains(ShadeFlag.Extract)) {
          addAction(module, ShadeAction.Extract)
          for (depList <- deps.get(module); dep <- depList) addAction(dep, ShadeAction.Extract)
        }
        if (configurations.contains(ShadeFlag.Shade) || configurations.contains(ShadeFlag.ShadeDeps)) {
          addAction(module, ShadeAction.Shade)
          for (depList <- deps.get(module); dep <- depList)
            addAction(dep, if (configurations.contains(ShadeFlag.ShadeDeps)) ShadeAction.Shade else ShadeAction.Extract)
          for (depList <- reverseDeps.get(module); dep <- depList) addAction(dep, ShadeAction.Shade)
        }
      }
    }

    classpath.map(x =>
      x -> x.get(moduleID.key)
        .flatMap(x => chosenActions.get(cleanModuleID(x)))
        .getOrElse(ShadeAction.Provided)
    )
  }

  private def calculateAction(action: PartialFunction[ShadeAction, ShadeAction]) = {
    val rawClasspath = annotatedClasspath.map(x => x.copy(_2 = action.applyOrElse(x._2, (y: ShadeAction) => y)))
    def extractClasspath(target: ShadeAction) = rawClasspath.filter(_._2 == target).map(_._1)
    ShadeClasspaths(
      extractClasspath(ShadeAction.NotShaded),
      extractClasspath(ShadeAction.Shade),
      extractClasspath(ShadeAction.Extract),
      shadePrefix
    )
  }

  lazy val deobfClasspaths = calculateAction { case ShadeAction.Extract => ShadeAction.NotShaded }
  lazy val obfClasspaths = calculateAction { case x => x }

  override def toString = s"ShadeInfo($deobfClasspaths, $obfClasspaths)"
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
  def generateDepsJar(classpaths: ShadeClasspaths, log: Logger = null) = {
    val shadeJars = classpaths.shade.map(x => JarData.load(x.data).stripSignatures)
    val shadeMapping = ShadeMapping((
      for (jar <- shadeJars; name <- jar.classes.keySet) yield name -> s"${classpaths.shadePrefix}/$name").toMap)
    val merged = JarData.mergeAll(shadeJars, log)
    val mapped = shadeMapping.mapJar(merged)
    mapped.manifest = new Manifest()
    mapped.manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    for (dep <- classpaths.extract) addExtractedDep(mapped, dep.data, dep.get(moduleID.key))
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
