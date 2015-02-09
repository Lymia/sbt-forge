package moe.lymia.sbt.forge

import sbt._
import asm._
import java.io._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ArrayBuffer}

import language._

object mapping {
  private val classNameRegex    = "([^ ]+)/([^ /]+)".r
  private val mapClassDescRegex = "(\\[*)L([^;]+);".r

  case class FunctionDesc(params: Seq[String], ret: String) {
    override def toString = "(" + params.fold("")(_ + _) + ")" + ret
    def map(f: String => String) = FunctionDesc(params.map(f), f(ret))
  }
  object DescParser extends scala.util.parsing.combinator.RegexParsers {
    override val skipWhitespace = false

    def arrayType : Parser[String] = ("[" ~> typeDesc) ^^ { case s => "["+s }
    def baseType  : Parser[String] = "[BCDFIJSZ]".r
    def classType : Parser[String] = "L" ~> "([^;]+)".r <~ ";"
    def typeDesc  : Parser[String] = arrayType | baseType | classType
    def returnType: Parser[String] = typeDesc | "V"

    def desc          = typeDesc *
    def methodDesc    = ("(" ~> desc <~ ")") ~ returnType ^^ { case a~b => FunctionDesc(a, b) }

    def parseDesc(s:String) = parseAll(desc, s) match {
      case Success(nodes, _)   => nodes
      case NoSuccess(err,next) => sys.error("Failed to parse descriptor \""+s+"\" at column "+next.pos.column+": "+err)
    }
    def parseMethodDesc(s:String) = parseAll(methodDesc, s) match {
      case Success(nodes, _)   => nodes
      case NoSuccess(err,next) => sys.error("Failed to parse method descriptor \""+s+"\" at column "+next.pos.column+": "+err)
    }
  }


  def splitClassName(name: String) = name match {
    case classNameRegex(owner, name) => (owner, name)
    case _ => (".", name)
  }
  def joinClassName(owner: String, name: String) =
    if(owner == ".") name
    else owner+"/"+name

  case class FieldSpec(owner: String, name: String)
  case class MethodSpec(owner: String, name: String, desc: String)
  class ForgeMapping(val packageMapping: mutable.Map[String, String]         = new HashMap[String, String],
                     val classMapping  : mutable.Map[String, String]         = new HashMap[String, String],
                     val fieldMapping  : mutable.Map[FieldSpec, FieldSpec]   = new HashMap[FieldSpec, FieldSpec],
                     val methodMapping : mutable.Map[MethodSpec, MethodSpec] = new HashMap[MethodSpec, MethodSpec]) {
    def mapClassName(name: String) = {
      val (owner, clname) = splitClassName(classMapping.getOrElse(name, name))
      joinClassName(packageMapping.getOrElse(owner, owner), clname)
    }
    def mapDescComponent(desc: String) = desc match {
      case mapClassDescRegex(numBracket, className) => numBracket + "L" + mapClassName(className) + ";"
      case _ => desc
    }
    def mapDesc(desc: String) = DescParser.parseDesc(desc).map(mapDescComponent _).fold("")(_ + _)
    def mapMethodDesc(desc: String) = DescParser.parseMethodDesc(desc).map(mapDescComponent _).toString

    def checkConsistancy() = {
      if(!(classMapping.values.toSet & classMapping.keySet).isEmpty)
        sys.error("Possible cycle in mapping.")
      for((FieldSpec(sOwner, sName), FieldSpec(tOwner, tName)) <- fieldMapping)
        if(mapClassName(sOwner) != tOwner)
          sys.error("Attempt to move field "+sName+" in "+mapClassName(sOwner)+" to "+tOwner)
      for((MethodSpec(sOwner, sName, sDesc), MethodSpec(tOwner, tName, tDesc)) <- methodMapping) {
        if(mapClassName(sOwner) != tOwner)
          sys.error("Attempt to move method "+sName+sDesc+" in "+mapClassName(sOwner)+" to "+tOwner)
        if(mapMethodDesc(sDesc) != tDesc)
          sys.error("Descriptor "+tDesc+" does not match expected descriptor "+mapMethodDesc(sDesc)+
                    " in method "+tOwner+"."+tName)
      }
    }
    def reverseMapping() =
      // TODO Add consistancy checking
      new ForgeMapping(packageMapping.map(_.swap), classMapping.map(_.swap),
                       fieldMapping.map(_.swap), methodMapping.map(_.swap))
    
    override def clone() = 
      new ForgeMapping(packageMapping.clone(), classMapping.clone(), 
                       fieldMapping.clone(), methodMapping.clone())
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
        mapping.packageMapping.put(source, target)
      case lineRegex("CL", CL(source, target)) => 
        mapping.classMapping.put(source, target)
      case lineRegex("FD", FD(sOwner, sName, tOwner, tName)) => 
        mapping.fieldMapping.put(FieldSpec(sOwner, sName), FieldSpec(tOwner, tName))
      case lineRegex("MD", MD(sOwner, sName, sDesc, tOwner, tName, tDesc)) => 
        mapping.methodMapping.put(MethodSpec(sOwner, sName, sDesc), MethodSpec(tOwner, tName, tDesc))
      case x if x.trim == "" => // ignore empty lines
      case x => sys.error("Could not parse SRG line: "+x)
    }
    
    mapping.checkConsistancy()

    mapping
  }

  
}
