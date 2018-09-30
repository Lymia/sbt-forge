package moe.lymia.forge.build

import java.util.Locale

import moe.lymia.forge.asm._
import moe.lymia.forge.Utils._
import moe.lymia.forge.mapper.Mapping
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import sbt._

import scala.collection.mutable

sealed abstract class AccessLevel(private val order: Int, val flag: Int,
                                  override val toString: String) extends Ordered[AccessLevel] {
  override def compare(o: AccessLevel): Int = this.order - o.order

  def transform(original: Int) =
    (original & ~AccessLevel.VisibilityModifiers) | max(AccessLevel.forModifier(original), this).flag
}
object AccessLevel {
  case object Public    extends AccessLevel(3, ACC_PUBLIC   , "public")
  case object Protected extends AccessLevel(2, ACC_PROTECTED, "protected")
  case object Default   extends AccessLevel(1, 0            , "default")
  case object Private   extends AccessLevel(0, ACC_PRIVATE  , "private")

  private val VisibilityModifiers = ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED
  def forModifier(mod: Int) = mod & VisibilityModifiers match {
    case ACC_PUBLIC    => Public
    case ACC_PROTECTED => Protected
    case 0             => Default
    case ACC_PRIVATE   => Private
    case _ => sys.error("Invalid access modifier found in class file!")
  }
}

sealed abstract class FinalStatus(private val order: Int,
                                  override val toString: String) extends Ordered[FinalStatus] {
  override def compare(o: FinalStatus): Int = this.order - o.order

  def transform(original: Int) = this match {
    case FinalStatus.RemoveFinal => original & ~ACC_FINAL
    case FinalStatus.NoChange    => original
    case FinalStatus.SetFinal    => original | ACC_FINAL
  }
}
object FinalStatus {
  case object RemoveFinal extends FinalStatus(2, "-f")
  case object SetFinal    extends FinalStatus(1, "+f")
  case object NoChange    extends FinalStatus(0, "")
}

final case class ATFlags(newAccess: AccessLevel, finalFlagChange: FinalStatus) {
  def transform(acc: Int) = finalFlagChange.transform(newAccess.transform(acc))

  def &&(other: ATFlags) =
    ATFlags(max(newAccess, other.newAccess), max(finalFlagChange, other.finalFlagChange))
  def isSupersetOf(other: ATFlags) =
    newAccess >= other.newAccess && finalFlagChange >= other.finalFlagChange

  override def toString = s"$newAccess$finalFlagChange"
}
object ATFlags {
  val Null = ATFlags(AccessLevel.Private, FinalStatus.NoChange)

  def apply(str: String): ATFlags = {
    val lcStr = str.toLowerCase(Locale.ENGLISH)
    val (access, finalStats) =
           if (lcStr.endsWith("-f")) (lcStr.substring(0, lcStr.length - 2), FinalStatus.RemoveFinal)
      else if (lcStr.endsWith("+f")) (lcStr.substring(0, lcStr.length - 2), FinalStatus.SetFinal)
      else                           (lcStr, FinalStatus.NoChange)
    ATFlags(access match {
      case "public"    => AccessLevel.Public
      case "protected" => AccessLevel.Protected
      case "default"   => AccessLevel.Default
      case "private"   => AccessLevel.Private
      case a           => sys.error(s"Could not parse access transformation: $str")
    }, finalStats)
  }
}

sealed trait ATTarget {
  private[build] val isSynthetic = false
  private[build] def remap(mapping: Mapping): ATTarget
}
object ATTarget {
  private def toJavaName(name: String) = name.replace('/', '.')

  case class Class(name: String) extends ATTarget {
    override def toString = toJavaName(name)
    private[build] override def remap(mapping: Mapping) =
      Class(mapping.map(name))
  }
  case class InnerClass(owner: String, name: String) extends ATTarget {
    override def toString = s"<Inner class entry: ${toJavaName(owner)}$$$name>"
    private[build] override val isSynthetic = true
    private[build] override def remap(mapping: Mapping) =
      InnerClass(mapping.map(owner), name)
  }
  case class Field(owner: String, name: String) extends ATTarget {
    override def toString = s"${toJavaName(owner)} $name"
    private[build] override def remap(mapping: Mapping) =
      Field(mapping.map(owner), mapping.mapPartialFieldName(owner, name))
  }
  case class Method(owner: String, method: MethodName) extends ATTarget {
    override def toString = s"${toJavaName(owner)} ${method.name}${method.desc}"
    private[build] override def remap(mapping: Mapping) =
      Method(mapping.map(owner), method.copy(name = mapping.mapMethodName(owner, method.name, method.desc)))
  }
  case class FieldWildcard(owner: String) extends ATTarget {
    override def toString = s"${toJavaName(owner)} *"
    private[build] override def remap(mapping: Mapping) =
      FieldWildcard(mapping.map(owner))
  }
  case class MethodWildcard(owner: String) extends ATTarget {
    override def toString = s"${toJavaName(owner)} *()"
    private[build] override def remap(mapping: Mapping) =
      MethodWildcard(mapping.map(owner))
  }
}

final case class AccessTransformer(transformations: Map[ATTarget, ATFlags]) {
  private def forTarget(target: ATTarget) = transformations.getOrElse(target, ATFlags.Null)

  private def transformClassAccess(name: String, access: Int) =
    forTarget(ATTarget.Class(name)).transform(access)
  private def transformInnerClassAccess(owner: String, name: String, access: Int) =
    forTarget(ATTarget.InnerClass(owner, name)).transform(access)
  private def transformFieldAccess(owner: String, name: FieldName, access: Int) =
    (forTarget(ATTarget.FieldWildcard(owner)) && forTarget(ATTarget.Field(owner, name.name))).transform(access)
  private def transformMethodAccess(owner: String, name: MethodName, access: Int) =
    (forTarget(ATTarget.MethodWildcard(owner)) && forTarget(ATTarget.Method(owner, name))).transform(access)

  private def makeVisitor(cv: ClassVisitor) = new ClassVisitor(ASM5, cv) {
    private var classNameField: String = _
    def className = {
      if (classNameField == null) sys.error("visit called before visitField or visitMethod!")
      classNameField
    }

    override def visit(version: Int, access: Int, name: String,
                       signature: String, superName: String, interfaces: Array[String]) = {
      this.classNameField = name
      super.visit(version, transformClassAccess(name, access), name,
                  signature, superName, interfaces)
    }
    override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) =
      super.visitInnerClass(name, outerName, innerName,
                            transformInnerClassAccess(className, innerName, access))
    override def visitField(access: Int, name: String, desc: String,
                            signature: String, value: Any) =
      super.visitField(transformFieldAccess(className, FieldName(name, desc), access),
                       name, desc, signature, value)
    override def visitMethod(access: Int, name: String, desc: String,
                             signature: String, exceptions: Array[String]) =
      super.visitMethod(transformMethodAccess(className, MethodName(name, desc), access),
                        name, desc, signature, exceptions)
  }
  private lazy val isClassMapped = transformations.keySet.map {
    case ATTarget.Class(name) => name
    case ATTarget.InnerClass(owner, _) => owner
    case ATTarget.Field(owner, _) => owner
    case ATTarget.Method(owner, _) => owner
    case ATTarget.FieldWildcard(owner) => owner
    case ATTarget.MethodWildcard(owner) => owner
  }
  def transformJar(jar: JarData) = jar.mapWithVisitor(this.makeVisitor, isClassMapped.contains)

  def getAtLines = transformations.filter(!_._1.isSynthetic).map(x => s"${x._2} ${x._1}").toSeq
  def writeTo(file: File) = IO.writeLines(file, "# Merged by sbt-forge" +: getAtLines)

  def remap(mapper: Mapping) =
    AccessTransformer(transformations.map(x => (x._1.remap(mapper), x._2)))
}
object AccessTransformer {
  private def toInternalName(name: String) = name.replace('.', '/')

  val MethodNameRegex = """([^(]+)(\([^)]*\).*)""".r
  private def parseLine(line: String) = {
    val trimmedLine = line.replaceAll("#.*", "").trim
    if (trimmedLine.isEmpty) Nil
    else trimmedLine.split(" +") match {
      case Array(access, className, "*()") =>
        Seq((ATTarget.MethodWildcard(toInternalName(className)), ATFlags(access)))
      case Array(access, className, "*") =>
        Seq((ATTarget.FieldWildcard(toInternalName(className)), ATFlags(access)))
      case Array(access, className, MethodNameRegex(methodName, methodDesc)) =>
        Seq((ATTarget.Method(toInternalName(className), MethodName(methodName, methodDesc)), ATFlags(access)))
      case Array(access, className, fieldName) =>
        Seq((ATTarget.Field(toInternalName(className), fieldName), ATFlags(access)))
      case Array(access, InnerClassNameRegex(outerName, innerName)) =>
        val transformation = ATFlags(access)
        Seq((ATTarget.Class(toInternalName(s"$outerName$$$innerName")), transformation),
            (ATTarget.InnerClass(toInternalName(outerName), innerName), transformation))
      case Array(access, className) =>
        Seq((ATTarget.Class(toInternalName(className)), ATFlags(access)))
      case a => sys.error(s"Could not parse line in access transformer: ${a.mkString(" ")}")
    }
  }
  private def mergeLines(lines: Seq[(ATTarget, ATFlags)]) = {
    val allLines        = new mutable.HashMap[ATTarget, ATFlags]()
    val fieldWildcards  = new mutable.HashMap[ATTarget.FieldWildcard , ATFlags]()
    val methodWildcards = new mutable.HashMap[ATTarget.MethodWildcard, ATFlags]()

    @inline def getLine(target: ATTarget) =
      allLines.getOrElse(target, ATFlags.Null)
    @inline def addLine(target: ATTarget, flags: ATFlags) =
      allLines.put(target, getLine(target) && flags)

    for ((target, flags) <- lines) target match {
      case target: ATTarget.FieldWildcard  => fieldWildcards .put(target, flags)
      case target: ATTarget.MethodWildcard => methodWildcards.put(target, flags)
      case _ =>
    }
    for ((target, flags) <- lines) target match {
      case ATTarget.Method(owner, name) =>
        if (!getLine(ATTarget.MethodWildcard(owner)).isSupersetOf(flags)) addLine(target, flags)
      case ATTarget.Field(owner, name) =>
        if (!getLine(ATTarget.FieldWildcard (owner)).isSupersetOf(flags)) addLine(target, flags)
      case _ => addLine(target, flags)
    }

    AccessTransformer(allLines.toMap)
  }

  def merge(transformers: Seq[AccessTransformer]) =
    mergeLines(transformers.flatMap(_.transformations))
  def parse(files: File*) =
    mergeLines(files.flatMap(x => IO.readLines(x)).flatMap(parseLine))
}
