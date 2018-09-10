package moe.lymia.sbt.forge

import sbt._
import asm._
import java.io._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ArrayBuffer}

import org.objectweb.asm._
import org.objectweb.asm.commons._

import language._

object mapping {
  private val classNameRegex    = "([^ ]+)/([^ /]+)".r
  def splitClassName(name: String) = name match {
    case classNameRegex(owner, name) => (owner, name)
    case _ => (".", name)
  }
  def joinClassName(owner: String, name: String) =
    if(owner == ".") name
    else owner+"/"+name

  case class FieldSpec (owner: String, name: String, desc: String)
  case class MethodSpec(owner: String, name: String, desc: String)
  class ForgeMapping(val packageMapping: mutable.Map[String, String]     = new HashMap[String, String],
                     val classMapping  : mutable.Map[String, String]     = new HashMap[String, String],
                     val fieldMapping  : mutable.Map[FieldSpec, String]  = new HashMap[FieldSpec, String],
                     val methodMapping : mutable.Map[MethodSpec, String] = new HashMap[MethodSpec, String]) extends Remapper {
    override def map(name: String) = classMapping.get(name) match {
      case Some(name) => name
      case None => 
        val (owner, clname) = splitClassName(name)
        joinClassName(packageMapping.getOrElse(owner, owner), clname)
    }
    override def mapFieldName(owner: String, name: String, desc: String) = 
      sys.error("not supported")
    override def mapMethodName(owner: String, name: String, desc: String) =
      sys.error("not supported")
    override def mapInvokeDynamicMethodName(name: String, desc: String) =
      sys.error("Uh, you know that you're supposed to write mods targeting Java 6, right?")


    def checkConsistancy() = {
      if((classMapping.values.toSet & classMapping.keySet).nonEmpty)
        sys.error("Possible cycle in mappings: "+(classMapping.values.toSet & classMapping.keySet))
    }
    def reverseMapping() =
      // TODO: Add check for duplicates
      new ForgeMapping(packageMapping.map(_.swap), classMapping.map(_.swap),
                       fieldMapping .map(x => FieldSpec (map(x._1.owner), x._2, mapMethodDesc(x._1.desc)) -> x._1.name), 
                       methodMapping.map(x => MethodSpec(map(x._1.owner), x._2, mapMethodDesc(x._1.desc)) -> x._1.name))
    
    override def clone() = 
      new ForgeMapping(packageMapping.clone(), classMapping.clone(), 
                       fieldMapping.clone(), methodMapping.clone())
  }

  def mappingFromSrgFile(ref: JarData, lines: Seq[String], log: Logger) = {
    val mapping = new ForgeMapping()

    val lineRegex = "([A-Z][A-Z]): (.*)".r.anchored
    val PK  = "([^ ]+) +([^ ]+)".r.anchored
    val CL  = "([^ ]+) +([^ ]+)".r.anchored
    val FD  = "([^ ]+)/([^ /]+) +([^ ]+)/([^ /]+)".r.anchored
    val MD  = "([^ ]+)/([^ /]+) +([^ ]+) +([^ ]+)/([^ /]+) +([^ ]+)".r.anchored
    lines.foreach {
      case lineRegex("PK", PK(source, target)) => 
        if(source != target) mapping.packageMapping.put(source, target)
      case lineRegex("CL", CL(source, target)) => 
        if(source != target) mapping.classMapping.put(source, target)
      case lineRegex("FD", FD(sOwner, sName, _, tName)) =>
        ref.classes.get(sOwner) match {
          case Some(cn) =>
            for((FieldName(name, desc), _) <- cn.fieldMap if name == sName)
              mapping.fieldMapping.put(FieldSpec(sOwner, sName, desc), tName)
          case None => log.warn("FD line encountered in class "+sOwner+", but class was not found in reference .jar!")
        }
      case lineRegex("MD", MD(sOwner, sName, sDesc, _, tName, _)) => 
        if(sName != tName) mapping.methodMapping.put(MethodSpec(sOwner, sName, sDesc), tName)
      case x if x.trim == "" => // ignore empty lines
      case x => sys.error("Could not parse SRG line: "+x)
    }
    
    mapping.checkConsistancy()
    mapping
  }
  def readCsvMappings(mapping: Seq[String]) =
    mapping.map(_.split(",").map(_.trim)).filter(_.length >= 2).map(a => a(0) -> a(1)).toMap
  def mappingFromConfFiles(ref: JarData, fields: Seq[String], methods: Seq[String]) = {
    val mapping        = new ForgeMapping()
    val fieldMappings  = readCsvMappings(fields)
    val methodMappings = readCsvMappings(methods)
    for((className, cn) <- ref.classes) {
      for((MethodName(name, desc), _) <- cn.methodMap;
          target                      <- methodMappings.get(name))
        mapping.methodMapping.put(MethodSpec(className, name, desc), target)
      for((FieldName(name, desc), _) <- cn.fieldMap;
          target                     <- fieldMappings.get(name))
        mapping.fieldMapping.put(FieldSpec(className, name, desc), target)
    }
    mapping
  }

  def readMapping(lines: Seq[String]) = {
    val mapping = new ForgeMapping()

    lines.map(_.replaceAll("#.*", "").trim).filter(!_.isEmpty).map(_.split(" +")) foreach {
      case Array("package", source, "->", target) => mapping.packageMapping.put(source, target)
      case Array("class"  , source, "->", target) => mapping.classMapping  .put(source, target)
      case Array("field"  , owner, name, desc, "->", target) =>
        mapping.fieldMapping .put(FieldSpec (owner, name, desc), target)
      case Array("method" , owner, name, desc, "->", target) =>
        mapping.methodMapping.put(MethodSpec(owner, name, desc), target)
      case x => sys.error("Could not parse mapping line: "+x.reduce(_ + " " + _))
    }
    
    mapping.checkConsistancy()
    mapping
  }
  def dumpMapping(os: OutputStream, mapping: ForgeMapping) {
    val out = new PrintStream(os)
    out.println("# This file is generated by sbt-forge.")
    for((source, target) <- mapping.packageMapping) out.println("package "+source+" -> "+target)
    for((source, target) <- mapping.classMapping  ) out.println("class "+source+" -> "+target)
    for((FieldSpec(owner, name, desc), target) <- mapping.fieldMapping)
      out.println("field "+owner+" "+name+" "+desc+" -> "+target)
    for((MethodSpec(owner, name, desc), target) <- mapping.methodMapping)
      out.println("method "+owner+" "+name+" "+desc+" -> "+target)
    out.close()
  }
}
