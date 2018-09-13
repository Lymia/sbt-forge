package moe.lymia.forge

import asm._
import org.objectweb.asm.Opcodes._
import sbt._

object AccessTransformer {
  val accessMask = ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED
  def transformAccess(acc: Int, target: String) = {
    val newAccess =      if(target.startsWith("public"   )) ACC_PUBLIC
                    else if(target.startsWith("private"  )) ACC_PRIVATE
                    else if(target.startsWith("protected")) ACC_PROTECTED
                    else                                    0
    val masked = acc & ~accessMask
    val mappedAccess = acc & accessMask match {
      case ACC_PRIVATE   => masked | newAccess
      case 0             => if(newAccess != ACC_PRIVATE                  ) masked | newAccess else acc
      case ACC_PROTECTED => if(newAccess != ACC_PRIVATE && newAccess != 0) masked | newAccess else acc
      case ACC_PUBLIC    => acc
      case _ => sys.error("Invalid access modifier found in class file!")
    }
         if(target.endsWith("-f")) mappedAccess & ~ACC_FINAL
    else if(target.endsWith("+f")) mappedAccess |  ACC_FINAL
    else                           mappedAccess
  }

  // TODO: Support old access transformer format?
  val methodNameRegex = """([^(]+)(\([^)]*\).*)""".r
  def applyAccessTransformers(inputJar: JarData, at: Seq[String], log: Logger) {
    def lookupClass(className: String) =
      inputJar.classes.get(className.replace(".", "/")) match {
        case Some(cn) => Some(cn)
        case None     => 
          log.warn("Access transformer defined for class "+className+", but class was not found.")
          None
      }
    at.map(_.replaceAll("#.*", "").trim).filter(!_.isEmpty).map(_.split(" +")) foreach {
      case Array(access, className, "*()") =>
        lookupClass(className) foreach { cn =>
          for(mn <- cn.methodMap.values) mn.access = transformAccess(mn.access, access)
        }
      case Array(access, className, "*") =>
        lookupClass(className) foreach { cn =>
          for(fn <- cn.fieldMap.values) fn.access = transformAccess(fn.access, access)
        }
      case Array(access, className, methodNameRegex(methodName, methodDesc)) =>
        lookupClass(className) foreach { cn =>
          val t = methodDesc.replace(".", "/")
          cn.methodMap.get(MethodName(methodName, if(t.endsWith(")")) t + "V" else t)) match {
            case Some(mn) => mn.access = transformAccess(mn.access, access)
            case None     =>
              log.warn("Access transformer defined for method "+className+"."+methodName+methodDesc+", but method was not found.")
          }
        }
      case Array(access, className, fieldName) =>
        lookupClass(className) foreach { cn =>
          val fields = cn.fieldMap.values.filter(_.name == fieldName).toSeq
          if(fields.isEmpty) log.warn("Access transformer defined for field "+className+"."+fieldName+", but no matching fields were found.")
          for(fn <- fields) fn.access = transformAccess(fn.access, access)
        }
      case Array(access, className) =>
        lookupClass(className) foreach { cn =>
          cn.access = transformAccess(cn.access, access)
        }
      case a => log.warn("Could not parse line in access transformer: "+a.reduce(_ + " " + _))
    }
  }
}
