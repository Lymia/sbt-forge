package moe.lymia.sbt.forge

import sbt._
import asm._
import mapping._

import java.util.Properties
import java.io.InputStream

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object Exceptor {
  def applyExceptorJson(inputJar: JarData, json: String, log: Logger) {
    Json.parse(json).as[Map[String, JsObject]] foreach { t =>
      val (name, data) = t
      inputJar.classes.get(name) match { 
        case Some(cn) =>
          (data \ "enclosingMethod").asOpt[JsObject] foreach { obj =>
            cn.outerClass = (obj \ "owner").as[String]
            cn.outerMethod = (obj \ "name").asOpt[String].getOrElse(null)
            cn.outerMethodDesc = (obj \ "desc").asOpt[String].getOrElse(null)
          }
          (data \ "innerClasses").asOpt[Seq[JsObject]] foreach { _ foreach { l =>
            val icn = new InnerClassNode((l \ "inner_class").as[String],
                                         (l \ "outer_class").asOpt[String].getOrElse(null),
                                         (l \ "inner_name").asOpt[String].getOrElse(null),
                                         (l \ "access").asOpt[String].map(x => Integer.parseInt(x, 16)).getOrElse(0))
            cn.innerClasses += icn
          }}
        case None => log.warn("Exceptor contains inner class definitions for class "+name+
                              ", but that class does not exist in the processed jar.")
      }
    }
  }

  def getStartNode(mn: MethodNode) = mn.instructions.getFirst match {
    case l: LabelNode => l
    case n =>
      val l = new LabelNode()
      if(n == null) mn.instructions.add(l)
      else mn.instructions.insertBefore(n, l)
      l
  }
  def getEndNode(mn: MethodNode) = mn.instructions.getLast match {
    case l: LabelNode => l
    case n =>
      val l = new LabelNode()
      if(n == null) mn.instructions.add(l)
      else mn.instructions.insert(n, l)
      l
  }

  def addParameters(name: String, mn: MethodNode, paramNames: Seq[String]) = {
    val startNode = getStartNode(mn)
    val endNode = getEndNode(mn)
    val isMethod = (mn.access & ACC_STATIC) == 0
    val params = paramNames.zip(Type.getArgumentTypes(mn.desc).map(_.getDescriptor))
    mn.localVariables = new ArrayBuffer[LocalVariableNode]
    var indexPos = 0
    for((name, t) <- if(isMethod) ("this", "L"+name+";") +: params else params) {
      mn.localVariables += new LocalVariableNode(name, t, null, startNode, endNode, indexPos)
      indexPos += (if(t == "J" || t == "Q") 2 else 1)
    }
  }

  val accessRegex    = """([^.]+)\.([^(]+)(\([^)]*\)[^-]+)-Access""".r
  val funcRegex      = """([^.]+)\.([^(]+)(\([^)]*\).+)""".r
  val funcValueRegex = """([^|]*)\|(.*)""".r
  def applyExcFile(inputJar: JarData, in: InputStream, log: Logger) {
    def getMethod(className: String, name: String, desc: String) =
      inputJar.classes.get(className).flatMap(_.methodMap.get(MethodName(name, desc)))

    val prop = new Properties()
    prop.load(in)
    for((key, value) <- prop) key match {
      case accessRegex(className, methodName, desc) =>
        getMethod(className, methodName, desc) match {
          case Some(mn) =>
            mn.access = mn.access & ~(ACC_PRIVATE | ACC_PROTECTED | ACC_PUBLIC)
            value match {
              case "PUBLIC"    => mn.access = mn.access | ACC_PUBLIC
              case "PROTECTED" => mn.access = mn.access | ACC_PROTECTED
              case "PRIVATE"   => mn.access = mn.access | ACC_PRIVATE
              case "DEFAULT"   =>
            }
          case None => log.warn("Exceptor definition defines access change for "+className+"."+methodName+desc+
                                ", but method was not found in input jar.")
        }
      case funcRegex(className, methodName, desc) =>
        getMethod(className, methodName, desc) match {
          case Some(mn) =>
            val funcValueRegex(exceptionString, parameters) = value
            if(exceptionString != "") mn.exceptions ++= exceptionString.split(",").map(_.replace(".", "/"))
            if(parameters != "") addParameters(className, mn, parameters.split(","))
          case None => log.warn("Exceptor definition defines exception information for "+className+"."+methodName+desc+
                                ", but method was not found in input jar.")
        }
      case _ =>
    }
  }
  val srgFuncNameRegex = "func_([0-9]+)_.*".r
  def addDefaultParameterNames(inputJar: JarData) {
    for((clname, cn) <- inputJar.classes;
        (MethodName(name, desc), mn) <- cn.methodMap) name match {
      case srgFuncNameRegex(id) =>
        if(mn.localVariables != null && mn.localVariables.isEmpty) {
          val paramCount = Type.getArgumentTypes(mn.desc).length
          val paramStart = if((mn.access & ACC_STATIC) == 0) 1 else 0
          addParameters(clname, mn, (0 until paramCount).map(n => "p_"+id+"_"+(n+paramStart)+"_"))
        }
      case _ =>
    }
  }

  def stripSynthetic(inputJar: JarData) {
    for((_, cn) <- inputJar.classes) if(cn.superName != "java/lang/Enum") {
      for(mn <- cn.methods) mn.access = mn.access & ~ACC_SYNTHETIC
      for(fn <- cn.fields ) fn.access = fn.access & ~ACC_SYNTHETIC
    }
  }
}
