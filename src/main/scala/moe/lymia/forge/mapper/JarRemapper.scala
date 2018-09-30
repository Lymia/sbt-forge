package moe.lymia.forge.mapper

import java.io.File

import moe.lymia.forge.asm._
import moe.lymia.forge.mapper.classpath._
import moe.lymia.forge.Utils._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons._
import org.objectweb.asm._
import sbt._

import scala.collection.mutable
import scala.collection.JavaConverters._

// TODO: Implement hybrid rename/bridge remapping.
object JarRemapper {
  private val ObjectClass = "java/lang/Object"

  // Map parameter names from MCP configs
  def mapParams(targetJar: JarData, params: File) {
    val paramMapping = Mapping.readCsvMappings(IO.readLines(params))
    for (cn     <- targetJar.allClasses;
         mn     <- cn.methodMap.values if mn.localVariables != null;
         lvn    <- mn.localVariables.asScala;
         target <- paramMapping.get(lvn.name)) lvn.name = target
  }

  // Field/method resolver
  private final class ResolvingMapper(searcher: ClasspathSearcher, mapper: Mapping) extends Remapper {
    private val resolveField: FieldSpec => Option[FieldSpec] = cachedFunction { fs =>
      // JVMS 5.4.3.2
      if (fs.owner.startsWith("[")) Some(fs)
      else {
        val cn = searcher.loadSymbols(fs.owner)
        if (cn.fieldMap.contains(FieldName(fs.name, fs.desc))) Some(fs)
        else if (cn.name == ObjectClass) None
        else cn.interfaces.asScala.flatMap(x => resolveField(FieldSpec(x, fs.name, fs.desc))).headOption orElse
             resolveField(FieldSpec(cn.superName, fs.name, fs.desc))
      }
    }
    private val resolveMethod: MethodSpec => Option[MethodSpec] = cachedFunction { ms =>
      // JVMS 5.4.3.3
      if (ms.owner.startsWith("[")) Some(ms)
      else {
        val cn = searcher.loadSymbols(ms.owner)
        if (cn.methodMap.contains(MethodName(ms.name, ms.desc))) Some(ms)
        else if (cn.name == ObjectClass) None
        else resolveMethod(MethodSpec(cn.superName, ms.name, ms.desc)) orElse
            // TODO: Consider implementing the maximally-specific class thing
            cn.interfaces.asScala.flatMap(x => resolveMethod(MethodSpec(x, ms.name, ms.desc))).headOption
      }
    }

    override def map(name: String) = mapper.map(name)
    override def mapFieldName(owner: String, name: String, desc: String) = {
      val resolved = resolveField(FieldSpec(owner, name, desc))
      if (resolved.isEmpty) sys.error(s"Validation error: Field $owner.$name : $desc could not be resolved.")
      mapper.mapFieldName(resolved.head)
    }
    override def mapMethodName(owner: String, name: String, desc: String) = {
      val resolved = resolveMethod(MethodSpec(owner, name, desc))
      if (resolved.isEmpty) sys.error(s"Validation error: Method $owner.$name$desc could not be resolved.")
      mapper.mapMethodName(resolved.head)
    }
  }

  // Method override resolver
  private final class OverrideResolver(searcher: ClasspathSearcher) {
    private def checkOverrideFrom(caller: String, owner: String, method: MethodName, access: Int) = {
      // JVMS 5.4.5
      val isStatic = (access & ACC_STATIC) != 0
      val overrides = !isStatic && (access & (ACC_PRIVATE | ACC_PROTECTED | ACC_PUBLIC) match {
        case ACC_PUBLIC | ACC_PROTECTED => true
        case 0                          => splitClassName(owner)._1 == splitClassName(caller)._1
        case ACC_PRIVATE                => false
        case _                          => sys.error(s"Illegal access modifier in $owner!")
      })
      if (overrides && (access & ACC_FINAL) != 0)
        sys.error(s"Validation error: Class $caller in ${searcher.classLocation(caller)} "+
                  s"attempts to override final method $owner.${method.name}${method.desc}")
      overrides
    }
    private def checkOverridden(subclass: String, superclass: String, mn: MethodName) = {
      val cn = searcher.loadSymbols(superclass, subclass)
      (if(cn.methodMap.contains(mn) && checkOverrideFrom(subclass, superclass, mn, cn.methodMap(mn).access))
         Set(superclass)
       else Set.empty) ++ classesOverriddenBy(MethodSpec(superclass, mn.name, mn.desc))
    }
    val classesOverriddenBy: MethodSpec => Set[String] = cachedFunction { ms =>
      if (ms.owner == ObjectClass || ms.owner.startsWith("[")) Set.empty
      else {
        val cn = searcher.loadSymbols(ms.owner)
        if ((cn.access & (ACC_PRIVATE | ACC_STATIC)) != 0) Set.empty
        else checkOverridden(ms.owner, cn.superName, MethodName(ms.name, ms.desc)) ++
             cn.interfaces.asScala.flatMap(i => checkOverridden(ms.owner, i, MethodName(ms.name, ms.desc)))
      }
    }
    def overrides(caller: String, owner: String, name: String, desc: String) =
      classesOverriddenBy(MethodSpec(caller, name, desc)).contains(owner)
    def isRelated(caller: String, owner: String, name: String, desc: String) =
      overrides(caller, owner, name, desc) || overrides(owner, caller, name, desc)
  }

  // Find all methods that could possibly be renamed.
  private def possibleRenames(searcher: ClasspathSearcher, mapping: Mapping,
                              classList: collection.Set[String]) = {
    val couldRename = mapping.methodMapping.keys.map(x => MethodName(x.name, x.desc)).toSet
    val candidates = new mutable.HashMap[MethodName, mutable.Set[String]] with mutable.MultiMap[MethodName, String]
    for (name <- classList) {
      val cn = searcher.loadSymbols(name)
      for ((_, mn) <- cn.methodMap if couldRename.contains(MethodName(mn.name, mn.desc)))
        candidates.addBinding(MethodName(mn.name, mn.desc), cn.name)
    }
    for ((MethodName(name, desc), set) <- candidates; owner <- set) yield MethodSpec(owner, name, desc)
  }

  // Collapse all methods related to each other via inheritance into sets.
  private def buildEquivalenceSets(searcher: ClasspathSearcher, resolver: OverrideResolver,
                                   mapping: Mapping) = {
    val setMembers = new mutable.HashMap[Int, mutable.Set[MethodSpec]] with mutable.MultiMap[Int, MethodSpec]
    val methodMembership = new mutable.HashMap[MethodSpec, Int]

    var currentName = 0
    val renamed = possibleRenames(searcher, mapping, searcher.allClasses).toSeq
    for (candidate <- renamed) {
      val name = currentName
      methodMembership.put(candidate, name)
      setMembers.addBinding(name, candidate)
      currentName += 1
    }

    @inline def mergeSets(a: Int, b: Int) = if (a != b) {
      val bMap = setMembers.remove(b).getOrElse(sys.error("attempt to merge already merged set"))
      for (bMember <- bMap) methodMembership.put(bMember, a)
      setMembers.getOrElse(a, sys.error("attempt to merge already merged set")) ++= bMap
    }
    for (candidate <- renamed;
         superclass <- resolver.classesOverriddenBy(candidate))
      mergeSets(methodMembership(candidate), methodMembership(candidate.copy(owner = superclass)))

    setMembers.values.map(_.toSet).toSeq
  }

  // Build the final mapping
  private def prettySeq[T](s: Iterable[T]) =
    if(s.isEmpty) "<nothing>"
    else if(s.tail.isEmpty) s.head.toString
    else s"[${s.map(_.toString).mkString(", ")}]"
  private def propagateMapping(searcher: ClasspathSearcher, resolver: OverrideResolver, mapping: Mapping,
                               log: Logger) = {
    val newMethodMappings = new mutable.HashMap[MethodSpec, String]()
    for (related <- buildEquivalenceSets(searcher, resolver, mapping)) {
      val foundMappings = related.toSeq.flatMap(spec => mapping.methodMapping.get(spec).map(map => spec -> map))
      if (foundMappings.nonEmpty) {
        val method = related.head
        if (foundMappings.map(_._2).toSet.size != 1) {
          val methodStr = s"${method.name}${method.desc}"
          log.error(s"Mapping conflict while finding methods related to $methodStr")
          log.error(s"  Classes involved: ${prettySeq(related.map(_.owner))}")
          log.error("  Conflicting mappings:")
          for ((MethodSpec(owner, _, _), newName) <- foundMappings)
            log.error(s"    $owner.$methodStr -> $owner.$newName${method.desc}")
          sys.error(s"Mapping conflict for $methodStr!")
        }

        val mapTo = foundMappings.head._2
        log.debug(s"Propagating mapping ${method.name}${method.desc} -> $mapTo")
        log.debug(s"  Classes mapped: ${prettySeq(related.toSeq.map(_.owner).sorted)}")

        for (spec <- related) newMethodMappings.put(spec, mapTo)
      }
    }
    mapping.copy(methodMapping = newMethodMappings.toMap)
  }

  private val MetafactoryHandle = new Handle(
    Opcodes.H_INVOKESTATIC, "java/lang/invoke/LambdaMetafactory", "metafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
    false
  )
  private val AltMetafactoryHandle = new Handle(
    Opcodes.H_INVOKESTATIC, "java/lang/invoke/LambdaMetafactory", "altMetafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
    false
  )
  def applyMapping(targetJar: JarData, searchPath: Seq[File], dependencies: Seq[File],
                   mapping: Mapping, log: Logger) = {
    log.info("Building class graph...")
    val searcher = new ClasspathSearcher(targetJar, searchPath ++ dependencies, log)
    val resolver = new OverrideResolver(searcher)

    log.info("Propergating method mapping...")
    val newMapping = propagateMapping(searcher, resolver, mapping, log)

    log.info("Mapping classes...")
    val mapper = new ResolvingMapper(searcher, newMapping)
    (mapping, targetJar.mapWithVisitor(cv => new ClassRemapper(cv, mapper) {
      // TODO: Figure out if there exist Scala-specific bootstrap methods we must care about.
      override def createMethodRemapper(mv: MethodVisitor): MethodVisitor = new MethodRemapper(mv, remapper) {
          override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: Object*): Unit = {
            log.debug(s"invokedynamic instruction found: [$name, $desc, $bsm, $bsmArgs]")
            bsm match {
              case MetafactoryHandle | AltMetafactoryHandle =>
                val lambdaType = Type.getReturnType(desc).getInternalName
                val lambdaMethodSignature = bsmArgs(0).asInstanceOf[Type].getDescriptor
                log.debug(s" - Found metafactory method for SAM: $lambdaType/$name$lambdaMethodSignature")
                super.visitInvokeDynamicInsn(
                  remapper.mapMethodName(lambdaType, name, lambdaMethodSignature), desc, bsm, bsmArgs: _*
                )
              case _ =>
                super.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs: _*)
            }
          }
      }
    }))
  }
}
