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

  case class FieldSpec(owner: String, name: String)
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
      fieldMapping.get(FieldSpec(owner, name)).getOrElse(name)
    override def mapMethodName(owner: String, name: String, desc: String) =
      methodMapping.get(MethodSpec(owner, name, desc)).getOrElse(name)
    override def mapInvokeDynamicMethodName(name: String, desc: String) =
      sys.error("Uh, you know that you're supposed to write mods targeting Java 6, right?")

    def checkConsistancy() = {
      if(!(classMapping.values.toSet & classMapping.keySet).isEmpty)
        sys.error("Possible cycle in mappings: "+(classMapping.values.toSet & classMapping.keySet))
    }
    def reverseMapping() = {
      // TODO: Add check for duplicates
      new ForgeMapping(packageMapping.map(_.swap), classMapping.map(_.swap),
                       fieldMapping.map(x => FieldSpec(map(x._1.name), x._2) -> x._1.name), 
                       methodMapping.map(x => MethodSpec(map(x._1.owner), x._2, mapMethodDesc(x._1.desc)) -> x._1.name))
    }
    
    override def clone() = 
      new ForgeMapping(packageMapping.clone(), classMapping.clone(), 
                       fieldMapping.clone(), methodMapping.clone())

    def visitor(cv: ClassVisitor) = new RemappingClassAdapter(cv, this)
  }

  def readMappingFromSrg(in: InputStream) = {
    val mapping = new ForgeMapping()

    val lineRegex = "([A-Z][A-Z]): (.*)".r.anchored
    val PK = "([^ ]+) +([^ ]+)".r.anchored
    val CL = "([^ ]+) +([^ ]+)".r.anchored
    val FD = "([^ ]+)/([^ /]+) +([^ ]+)/([^ /]+)".r.anchored
    val MD = "([^ ]+)/([^ /]+) +([^ ]+) +([^ ]+)/([^ /]+) +([^ ]+)".r.anchored
    IO.readLines(new BufferedReader(new InputStreamReader(in))).foreach {
      case lineRegex("PK", PK(source, target)) => 
        if(source != target) mapping.packageMapping.put(source, target)
      case lineRegex("CL", CL(source, target)) => 
        if(source != target) mapping.classMapping.put(source, target)
      case lineRegex("FD", FD(sOwner, sName, _, tName)) => 
        if(sName != tName) mapping.fieldMapping.put(FieldSpec(sOwner, sName), tName)
      case lineRegex("MD", MD(sOwner, sName, sDesc, _, tName, _)) => 
        if(sName != tName) mapping.methodMapping.put(MethodSpec(sOwner, sName, sDesc), tName)
      case x if x.trim == "" => // ignore empty lines
      case x => sys.error("Could not parse SRG line: "+x)
    }
    
    mapping.checkConsistancy()

    mapping
  }
}
