package moe.lymia.forge.mapper

import java.io.File

import moe.lymia.forge.asm._
import moe.lymia.forge.mapper.classpath._
import moe.lymia.forge.mapper.mapping._
import moe.lymia.forge.Utils._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons.{Remapper, RemappingClassAdapter, RemappingMethodAdapter}
import org.objectweb.asm._
import sbt.Logger

import scala.collection.mutable
import scala.collection.JavaConverters._

object Renamer {
  // Fix mapping to include inner classes included with Forge
  val splitNameRegex = """^(.*)\$([^$]+)$""".r
  def findRemappableInnerClass(targetClasses: mutable.Map[String, ClassNodeWrapper],
                               mapping: ForgeMapping, log: Logger) {
    for((name, cn) <- targetClasses           if  mapping.classMapping.contains(name);
        icn        <- cn.innerClasses.asScala if !mapping.classMapping.contains(icn.name) &&
                                                  icn.name.startsWith(s"$name$$")) {
        val newName = s"${mapping.classMapping(name)}${icn.name.substring(name.length)}"
        log.debug(s"Adding mapping for inner class ${icn.name} to $newName")
        mapping.classMapping.put(icn.name, newName)
    }
  }

  // Map parameter names from MCP configs
  def mapParams(targetJar: JarData, params: Seq[String]) {
    val paramMapping = readCsvMappings(params)
    for((_, cn) <- targetJar.classes;
        mn      <- cn.methodMap.values if mn.localVariables != null;
        lvn     <- mn.localVariables.asScala;
        target  <- paramMapping.get(lvn.name))
      lvn.name = target
  }

  // Method/Field resolver
  class InheritenceResolver(searcher: ClasspathSearcher) {
    private def canOverrideFrom(owner: String, caller: String, access: Int) =
      access & (ACC_PRIVATE | ACC_PROTECTED | ACC_PUBLIC) match {
        case ACC_PUBLIC | ACC_PROTECTED => true
        case 0                          => splitClassName(owner)._1 == splitClassName(caller)._1
        case ACC_PRIVATE                => false
        case _ => sys.error(s"Illegal access modifier in $owner!")
      }
    private def checkOverrides(sub: String, sup: String, mn: MethodName) =
      searcher.resolve(sup).map(cn =>
        // TODO: Cut inheritence chain if canOverrideFrom fails
        (if(cn.methodMap.contains(mn) && canOverrideFrom(sup, sub, cn.methodMap(mn).access)) Set(sup)
         else Set()) ++ getOverrides(MethodSpec(sup, mn.name, mn.desc))
      ).getOrElse(Set())
    val getOverrides: MethodSpec => Set[String] = cachedFunction { ms =>
      searcher.resolve(ms.owner).map(cn =>
        checkOverrides(ms.owner, cn.superName, MethodName(ms.name, ms.desc)) ++
        cn.interfaces.asScala.flatMap(i => checkOverrides(ms.owner, i, MethodName(ms.name, ms.desc)))
      ).getOrElse(Set())
    }
    def overrides(caller: String, owner: String, name: String, desc: String) =
      getOverrides(MethodSpec(caller, name, desc)).contains(owner)
    def isRelated(caller: String, owner: String, name: String, desc: String) =
      overrides(caller, owner, name, desc) || overrides(owner, caller, name, desc)

    val resolveField: FieldSpec => Option[FieldSpec] = cachedFunction { fs =>
      searcher.resolve(fs.owner).flatMap { cn =>
        if(cn.fieldMap.contains(FieldName(fs.name, fs.desc))) Some(fs)
        else cn.interfaces.asScala.map(x => resolveField(FieldSpec(x, fs.name, fs.desc))).find(_.isDefined).flatten orElse
             resolveField(FieldSpec(cn.superName, fs.name, fs.desc))
      }
    }
    val resolveMethod: MethodSpec => Option[MethodSpec] = cachedFunction { ms =>
      searcher.resolve(ms.owner).flatMap { cn =>
        if(cn.methodMap.contains(MethodName(ms.name, ms.desc))) Some(ms)
        else resolveMethod(MethodSpec(cn.superName, ms.name, ms.desc)) orElse
             cn.interfaces.asScala.map(x => resolveMethod(MethodSpec(x, ms.name, ms.desc))).find(_.isDefined).flatten
      }
    }
  }

  // Mapping extension code
  def findRenamingCandidates(searcher: ClasspathSearcher, mapping: ForgeMapping,
                             classList: Seq[String], log: Logger) = {
    val couldRename = mapping.methodMapping.keys.map(x => MethodName(x.name, x.desc)).toSet
    val candidates = new mutable.HashMap[MethodName, mutable.Set[String]] with mutable.MultiMap[MethodName, String]
    for(name <- classList) searcher.resolve(name) match {
      case Some(cn) =>
        for((name, mn) <- cn.methodMap if couldRename.contains(MethodName(mn.name, mn.desc)))
          if((mn.access & ACC_STATIC) != ACC_STATIC && (mn.access & ACC_PRIVATE) != ACC_PRIVATE)
            candidates.addBinding(MethodName(mn.name, mn.desc), cn.name)
      case None => sys.error(s"Class $name not found while searching for rename candidates.")
    }
    candidates.toMap.mapValues(_.toSet)
  }
  private def prettySeq[T](s: Iterable[T]) =
    if(s.isEmpty) "<nothing>"
    else if(s.tail.isEmpty) s.head.toString
    else s"[${s.map(_.toString).mkString(", ")}]"
  def buildEquivalenceSets(methodName: String, methodDesc: String, candidates: Set[String],
                           inheritence: InheritenceResolver, mapping: ForgeMapping, log: Logger) = {
    def resolve     (name: String) = mapping.methodMapping.get(MethodSpec(name, methodName, methodDesc))
    val (mapped, nonMapped) = candidates.partition(x => resolve(x).isDefined)
    val equivalenceMap = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
    for(name <- mapped) equivalenceMap.addBinding(resolve(name).get, name)

    def mappingStep(remaining: Seq[String], mappingFunction: (String, String) => Boolean) = {
      val (mapped, left) = remaining.map(name =>
        (name, equivalenceMap.filter(t => t._2.exists(x => mappingFunction(x, name))).keys)).partition(_._2.nonEmpty)
      for((name, inSets) <- mapped) {
        if(inSets.size > 1) {
          log.error(s"Mapping conflict while finding methods related to $methodName$methodDesc")
          log.error(s"  Currently processing class: $name")
          log.error(s"  Conflicting target names: ${prettySeq(inSets)}")
          log.error("  Relevant mapping sets:")
          for(set <- inSets)
            log.error(s"    $set in ${prettySeq(equivalenceMap(set).toSeq)}")
          sys.error(s"Mapping conflict while procesing $methodName$methodDesc!")
        } else equivalenceMap.addBinding(inSets.head, name)
      }
      left.map(_._1)
    }

    val noDirectMapping = mappingStep(nonMapped.toSeq, (a, b) => inheritence.overrides(b, a, methodName, methodDesc))
    @annotation.tailrec def mapRecursive(candidates: Seq[String]): Seq[String] = {
      val remaining = mappingStep(candidates, (a, b) => inheritence.isRelated(a, b, methodName, methodDesc))
      if(remaining == candidates || remaining.isEmpty) remaining
      else mapRecursive(remaining)
    }
    val remaining = mapRecursive(noDirectMapping)

    (nonMapped.toSet, remaining.toSet, noDirectMapping.toSet, equivalenceMap.toMap.mapValues(_.toSet))
  }
  def applyMapping(targetJar: JarData, searchPath: Seq[File], dependencies: Seq[File], imapping: ForgeMapping, log: Logger) = {
    val mapping = imapping.clone()

    log.info("Building class graph...")
    val searcher    = new ClasspathSearcher(targetJar, searchPath ++ dependencies, log)
    val inheritence = new InheritenceResolver(searcher)

    log.info("Propergating method mapping...")
    val seeds =
      targetJar.classes.keys.toSeq ++
      searchPath.flatMap(x => searcher.sourceContents(x).getOrElse(sys.error(s"Source $x not found in classpath??")))
    val renameCandidates = findRenamingCandidates(searcher, mapping, seeds, log)
    renameCandidates.foreach { t =>
      val (MethodName(name, desc), candidates) = t
      val (noMapping, remaining, noDirectMapping, equivalenceMap) =
        buildEquivalenceSets(name, desc, candidates, inheritence, mapping, log)
      if(remaining.nonEmpty)
        log.debug(s"Classes ${prettySeq(remaining)} contain an method $name$desc, but, no relationship to remapped methods was found.")
      for((target, classes) <- equivalenceMap) {
        val sourceClasses   = classes -- noMapping
        val directClasses   = classes -- noDirectMapping -- sourceClasses
        val indirectClasses = classes -- directClasses -- sourceClasses
        val newMappings     = classes intersect noMapping

        val externalIndirect = indirectClasses.filter(x => !targetJar.classes.contains(x))
        if(externalIndirect.nonEmpty) {
          log.warn(s"The remapping $name$desc -> $target will propagate through external dependencies!")
          log.warn("This is very likely to cause issues.")
          log.warn(s"External classes: ${externalIndirect.map(x => x + " in " + searcher.classLocationStr(x)).mkString(", ")}")
        }

        if(directClasses.nonEmpty)
          log.debug(s"Propergating remapping $name$desc -> $target from ${prettySeq(sourceClasses)} to ${prettySeq(directClasses)}.")
        if(indirectClasses.nonEmpty)
          log.debug(s"Indirectly propergating remapping $name$desc -> $target from ${prettySeq(sourceClasses)} to ${prettySeq(indirectClasses)}.")

        for(n <- newMappings) mapping.methodMapping.put(MethodSpec(n, name, desc), target)
      }
    }

    log.info("Mapping classes...")
    val mapper = new Remapper() {
      override def map(name: String) = mapping.map(name)
      override def mapFieldName(owner: String, name: String, desc: String) =
        inheritence.resolveField(FieldSpec(owner, name, desc)).flatMap(fs => mapping.fieldMapping.get(fs)).getOrElse(name)
      override def mapMethodName(owner: String, name: String, desc: String) =
        if(owner.startsWith("[")) name
        else inheritence.resolveMethod(MethodSpec(owner, name, desc)).flatMap(ms => mapping.methodMapping.get(ms)).getOrElse(name)
    }

    // TODO: Use RemappingAdapter when we update sbt and fix the dep issue
    (mapping, targetJar.mapWithVisitor(cv => new RemappingClassAdapter(cv, mapper) {
      val MetafactoryHandle = new Handle(
        Opcodes.H_INVOKESTATIC, "java/lang/invoke/LambdaMetafactory", "metafactory",
        "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
      )
      val AltMetafactoryHandle = new Handle(
        Opcodes.H_INVOKESTATIC, "java/lang/invoke/LambdaMetafactory", "altMetafactory",
        "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
      )

      // TODO: Figure out if there exist other bootstrap methods that Scala code must care about.
      override def createRemappingMethodAdapter(access: Int, newDesc: String, mv: MethodVisitor): MethodVisitor =
        new RemappingMethodAdapter(access, newDesc, mv, remapper) {
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
