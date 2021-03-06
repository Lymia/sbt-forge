package moe.lymia.forge.mapper

import java.io.{FileOutputStream, PrintStream}

import moe.lymia.forge.Utils._
import moe.lymia.forge.asm._
import org.objectweb.asm.commons.Remapper
import play.api.libs.json._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable

final case class PartialFieldSpec(owner: String, name: String)
final case class FieldSpec       (owner: String, name: String, desc: String)
final case class MethodSpec      (owner: String, name: String, desc: String)

final case class Mapping(packageMapping: Map[String, String], classMapping: Map[String, String],
                         fieldMapping: Map[FieldSpec, String], methodMapping: Map[MethodSpec, String]
                        ) extends Remapper {
  private lazy val partialFieldMapping = {
    val mappings =
      new mutable.HashMap[PartialFieldSpec, mutable.Set[String]]() with mutable.MultiMap[PartialFieldSpec, String]
    for ((field, mapping) <- fieldMapping)
      mappings.addBinding(PartialFieldSpec(field.owner, field.name), mapping)
    mappings
  }
  def mapPartialFieldName(map: PartialFieldSpec) = {
    val result = partialFieldMapping.getOrElse(map, Set.empty)
    if (result.isEmpty) map.name
    else if (result.size == 1) result.head
    else sys.error(s"Validation error: Partial field name ${map.owner}.${map.name} is ambigious.")
  }
  def mapPartialFieldName(owner: String, name: String): String =
    mapPartialFieldName(PartialFieldSpec(owner, name))

  override def map(name: String) = classMapping.get(name) match {
    case Some(name) => name
    case None =>
      val (owner, clname) = splitClassName(name)
      joinClassName(packageMapping.getOrElse(owner, owner), clname)
  }

  def mapFieldName(fs: FieldSpec) = fieldMapping.getOrElse(fs, fs.name)
  override def mapFieldName(owner: String, name: String, desc: String) =
    fieldMapping.getOrElse(FieldSpec(owner, name, desc), name)

  def mapMethodName(ms: MethodSpec) = methodMapping.getOrElse(ms, ms.name)
  override def mapMethodName(owner: String, name: String, desc: String) =
    methodMapping.getOrElse(MethodSpec(owner, name, desc), name)

  def reverseMapping() =
    // TODO: Add check for duplicates
    Mapping(packageMapping.map(_.swap), classMapping.map(_.swap),
            fieldMapping .map(x => FieldSpec (map(x._1.owner), x._2, map          (x._1.desc)) -> x._1.name),
            methodMapping.map(x => MethodSpec(map(x._1.owner), x._2, mapMethodDesc(x._1.desc)) -> x._1.name))
  def stripTrivial() =
    Mapping(packageMapping.filter(x => x._1 != x._2), classMapping.filter(x => x._1 != x._2),
            fieldMapping.filter(x => x._1.name != x._2), methodMapping.filter(x => x._1.name != x._2))

  // Output methods
  def writeSrgMapping(target: File) {
    val out = new PrintStream(new FileOutputStream(target))
    try {
      for((source, target) <- packageMapping)
        out.println(s"PK: $source $target")
      for((source, target) <- classMapping)
        out.println(s"CL: $source $target")
      for((FieldSpec(owner, name, _), target) <- fieldMapping)
        out.println(s"FD: $owner/$name ${map(owner)}/$target")
      for((MethodSpec(owner, name, desc), target) <- methodMapping)
        out.println(s"MD: $owner/$name $desc ${map(owner)}/$target ${mapMethodDesc(desc)}")
    } finally {
      out.close()
    }
  }
  def writeCachedMapping(target: File) = {
    import Mapping._
    IO.write(target, Json.obj(
      "packageMapping" -> packageMapping, "classMapping" -> classMapping,
      "fieldMapping" -> fieldMapping.toSeq, "methodMapping" -> methodMapping.toSeq,
    ).toString())
  }

  // Helper methods for various compile tasks
  def findRemappableInnerClass(jar: JarData, log: Logger) = {
    val newMappings = new mutable.HashMap[String, String]()
    for(cn  <- jar.allClasses          if  classMapping.contains(cn.name);
        icn <- cn.innerClasses.asScala if !classMapping.contains(icn.name) && icn.name.startsWith(s"${cn.name}$$")) {
        val newName = s"${classMapping(cn.name)}${icn.name.substring(cn.name.length)}"
        log.debug(s"Adding mapping for inner class ${icn.name} to $newName")
        newMappings.put(icn.name, newName)
    }
    copy(classMapping = classMapping ++ newMappings)
  }

  // Check mappings for consistancy
  if((classMapping.values.toSet & classMapping.keySet).nonEmpty)
    sys.error(s"Possible cycle in mappings: ${classMapping.values.toSet & classMapping.keySet}")
}
object Mapping {
  private implicit val fieldSpecFormat = Json.format[FieldSpec]
  private implicit val methodSpecFormat = Json.format[MethodSpec]

  private[mapper] def readCsvMappings(mapping: Seq[String]) =
    mapping.tail.map(_.split(",").map(_.trim)).filter(_.length >= 2).map(a => a(0) -> a(1)).toMap

  def readMcpMapping(ref: JarData, fieldsFile: File, methodsFile: File) = {
    // TODO: Check for mappings defined but not used
    val csvFieldMappings  = readCsvMappings(IO.readLines(fieldsFile))
    val csvMethodMappings = readCsvMappings(IO.readLines(methodsFile))

    val fieldMappings =
      for (cn <- ref.allClasses;
           (FieldName(name, desc), _) <- cn.fieldMap;
           target <- csvFieldMappings.get(name)) yield (FieldSpec(cn.name, name, desc), target)
    val methodMappings =
      for (cn <- ref.allClasses;
           (MethodName(name, desc), _) <- cn.methodMap;
           target <- csvMethodMappings.get(name)) yield (MethodSpec(cn.name, name, desc), target)

    Mapping(Map(), Map(), fieldMappings.toMap, methodMappings.toMap).stripTrivial()
  }

  private val SRGLineRegex = "([A-Z][A-Z]): (.*)".r.anchored
  private val PK = "([^ ]+) +([^ ]+)".r.anchored
  private val CL = "([^ ]+) +([^ ]+)".r.anchored
  private val FD = "([^ ]+)/([^ /]+) +([^ ]+)/([^ /]+)".r.anchored
  private val MD = "([^ ]+)/([^ /]+) +([^ ]+) +([^ ]+)/([^ /]+) +([^ ]+)".r.anchored
  def readSrgMapping(ref: JarData, file: File, log: Logger) = {
    val packageMapping = new mutable.HashMap[String, String]()
    val classMapping   = new mutable.HashMap[String, String]()
    val fieldMapping   = new mutable.HashMap[FieldSpec, String]()
    val methodMapping  = new mutable.HashMap[MethodSpec, String]()

    IO.readLines(file).foreach {
      case SRGLineRegex("PK", PK(source, target)) =>
        if(source != target) packageMapping.put(source, target)
      case SRGLineRegex("CL", CL(source, target)) =>
        if(source != target) classMapping.put(source, target)
      case SRGLineRegex("FD", FD(sOwner, sName, _, tName)) =>
        ref.getClass(sOwner) match {
          case Some(cn) =>
            for((FieldName(name, desc), _) <- cn.fieldMap if name == sName)
              fieldMapping.put(FieldSpec(sOwner, sName, desc), tName)
          case None => log.warn(s"FD line encountered in class $sOwner, but class was not found in reference .jar!")
        }
      case SRGLineRegex("MD", MD(sOwner, sName, sDesc, _, tName, _)) =>
        if(sName != tName) methodMapping.put(MethodSpec(sOwner, sName, sDesc), tName)
      case x if x.trim == "" => // ignore empty lines
      case x => sys.error(s"Could not parse SRG line: $x")
    }

    Mapping(packageMapping.toMap, classMapping.toMap, fieldMapping.toMap, methodMapping.toMap).stripTrivial()
  }

  def readCachedMapping(file: File) = {
    val json = Json.parse(IO.readBytes(file))
    Mapping((json \ "packageMapping").as[Map[String, String]],
            (json \ "classMapping").as[Map[String, String]],
            (json \ "fieldMapping").as[Seq[(FieldSpec, String)]].toMap,
            (json \ "methodMapping").as[Seq[(MethodSpec, String)]].toMap)
  }
}