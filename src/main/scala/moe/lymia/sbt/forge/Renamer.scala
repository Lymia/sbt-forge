package moe.lymia.sbt.forge

import sbt._
import asm._
import mapping._
import classpath._

import java.io.File

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.commons._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, MultiMap}

object Renamer {
  // Fix mapping to include inner classes included with Forge
  val splitNameRegex = """^(.*)\$([^$]+)$""".r
  def findRemappableInnerClass(targetClasses: mutable.Map[String, ClassNodeWrapper], mapping: ForgeMapping, log: Logger) {
    for((name, cn) <- targetClasses   if  mapping.classMapping.contains(name);
        icn        <- cn.innerClasses if !mapping.classMapping.contains(icn.name) &&
                                          icn.name.startsWith(name+"$")) {
        val newName = mapping.classMapping(name) + icn.name.substring(name.length)
        log.debug("Adding mapping for inner class "+icn.name+" to "+newName)
        mapping.classMapping.put(icn.name, newName)
    }
  }

  // Map parameter names from MCP configs
  def mapParams(targetJar: JarData, params: Seq[String]) {
    val paramMapping = readCsvMappings(params)
    for((_, cn) <- targetJar.classes;
        mn      <- cn.methods if mn.localVariables != null;
        lvn     <- mn.localVariables;
        target  <- paramMapping.get(lvn.name))
      lvn.name = target
  }

  // Method/Field resolver
  class InheritenceResolver(searcher: ClasspathSearcher) {
    private def canOverrideFrom(owner: String, caller: String, access: Int) = 
      (access & (ACC_PRIVATE | ACC_PROTECTED | ACC_PUBLIC)) match {
        case ACC_PUBLIC | ACC_PROTECTED => true
        case 0                          => splitClassName(owner)._1 == splitClassName(caller)._1
        case ACC_PRIVATE                => false
        case _ => sys.error("Illegal access modifier in "+owner+"!")
      }
    private def checkOverrides(sub: String, sup: String, mn: MethodName) = 
      searcher.resolve(sup).map(cn =>
        // TODO: Cut inheritence chain if canOverrideFrom fails
        (if(cn.methodMap.contains(mn) && canOverrideFrom(sup, sub, cn.methodMap(mn).access)) Set(sup) 
         else Set()) ++ getOverrides(MethodSpec(sup, mn.name, mn.desc))
      ).getOrElse(Set())
    val getOverrides: MethodSpec => Set[String] = cacheFunction { ms =>
      searcher.resolve(ms.owner).map(cn =>
        checkOverrides(ms.owner, cn.superName, MethodName(ms.name, ms.desc)) ++
        cn.interfaces.flatMap(i => checkOverrides(ms.owner, i, MethodName(ms.name, ms.desc)))
      ).getOrElse(Set())
    }
    def overrides(caller: String, owner: String, name: String, desc: String) =
      getOverrides(MethodSpec(caller, name, desc)).contains(owner)
    def isRelated(caller: String, owner: String, name: String, desc: String) =
      overrides(caller, owner, name, desc) || overrides(owner, caller, name, desc)

    val resolveField: FieldSpec => Option[FieldSpec] = cacheFunction { fs =>
      searcher.resolve(fs.owner).flatMap { cn =>
        if(cn.fieldMap.contains(FieldName(fs.name, fs.desc))) Some(fs)
        else cn.interfaces.map(x => resolveField(FieldSpec(x, fs.name, fs.desc))).find(!_.isEmpty).flatten orElse
             resolveField(FieldSpec(cn.superName, fs.name, fs.desc))
      }
    }
    val resolveMethod: MethodSpec => Option[MethodSpec] = cacheFunction { ms =>
      searcher.resolve(ms.owner).flatMap { cn =>
        if(cn.methodMap.contains(MethodName(ms.name, ms.desc))) Some(ms)
        else resolveMethod(MethodSpec(cn.superName, ms.name, ms.desc)) orElse 
             cn.interfaces.map(x => resolveMethod(MethodSpec(x, ms.name, ms.desc))).find(!_.isEmpty).flatten
      }
    }
  }

  // Mapping extension code
  def findRenamingCandidates(searcher: ClasspathSearcher, mapping: ForgeMapping,
                             classList: Seq[String], log: Logger) = {
    val couldRename = mapping.methodMapping.keys.map(x => MethodName(x.name, x.desc)).toSet
    val candidates = new HashMap[MethodName, mutable.Set[String]] with MultiMap[MethodName, String]
    for(name <- classList) searcher.resolve(name) match {
      case Some(cn) =>
        for((name, mn) <- cn.methodMap if couldRename.contains(MethodName(mn.name, mn.desc)))
          if((mn.access & ACC_STATIC) != ACC_STATIC && (mn.access & ACC_PRIVATE) != ACC_PRIVATE)
            candidates.addBinding(MethodName(mn.name, mn.desc), cn.name)
      case None => sys.error("Class "+name+" not found while searching for rename candidates.")
    }
    candidates.toMap.mapValues(_.toSet)
  }
  private def prettySeq[T](s: Iterable[T]) = 
    if(s.size == 0) "<nothing>"
    else if(s.tail.isEmpty) s.head.toString
    else "["+s.map(_.toString).reduce(_ + "," + _)+"]"
  def buildEquivalenceSets(methodName: String, methodDesc: String, candidates: Set[String], 
                           inheritence: InheritenceResolver, mapping: ForgeMapping, log: Logger) = {
    def resolve     (name: String) = mapping.methodMapping.get(MethodSpec(name, methodName, methodDesc))
    val (mapped, nonMapped) = candidates.partition(x => !resolve(x).isEmpty)
    val equivalenceMap = new HashMap[String, mutable.Set[String]] with MultiMap[String, String]
    for(name <- mapped) equivalenceMap.addBinding(resolve(name).get, name)

    def mappingStep(remaining: Seq[String], mappingFunction: (String, String) => Boolean) = {
      val (mapped, left) = remaining.map(name => 
        (name, equivalenceMap.filter(t => t._2.exists(x => mappingFunction(x, name))).map(_._1))).partition(_._2.size > 0)
      for((name, inSets) <- mapped) {
        if(inSets.size > 1) {
          log.error("Mapping conflict while finding methods related to "+methodName+methodDesc)
          log.error("  Currently processing class: "+name)
          log.error("  Conflicting target names: "+inSets.reduce(_ + ", " + _))
          log.error("  Relevant mapping sets:")
          for(set <- inSets)
            log.error("    "+set+" in "+prettySeq(equivalenceMap(set).toSeq))
          sys.error("Mapping conflict while procesing "+methodName+methodDesc+"!")
        } else equivalenceMap.addBinding(inSets.head, name)
      }
      left.map(_._1)
    }

    val noDirectMapping = mappingStep(nonMapped.toSeq, (a, b) => inheritence.overrides(b, a, methodName, methodDesc))
    @annotation.tailrec def mapRecursive(candidates: Seq[String]): Seq[String] = {
      val remaining = mappingStep(candidates, (a, b) => inheritence.isRelated(a, b, methodName, methodDesc))
      if(remaining == candidates || remaining.length == 0) remaining
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
    val seeds = targetJar.classes.keys.toSeq ++ searchPath.flatMap(x => searcher.sourceContents(x).getOrElse(sys.error("Source "+x+" not found in classpath??")))
    val renameCandidates = findRenamingCandidates(searcher, mapping, seeds, log)
    var givenIndirectMappingLecture = false
    renameCandidates.foreach { t =>
      val (MethodName(name, desc), candidates) = t
      val (noMapping, remaining, noDirectMapping, equivalenceMap) = 
        buildEquivalenceSets(name, desc, candidates, inheritence, mapping, log) 
      if(remaining.size > 0)
        log.debug("Classes "+prettySeq(remaining)+" contain an method "+name+desc+", but, no relationship to remapped methods was found.")
      for((target, classes) <- equivalenceMap) {
        val sourceClasses   = classes -- noMapping
        val directClasses   = classes -- noDirectMapping -- sourceClasses
        val indirectClasses = classes -- directClasses -- sourceClasses
        val newMappings     = classes intersect noMapping

        val externalIndirect = indirectClasses.filter(x => !targetJar.classes.contains(x))
        // XXX: Should this be an outright error?
        if(externalIndirect.size > 0) {
          val externalClassLine = 
            "External classes: "+externalIndirect.map(x => x + " in " + searcher.classLocation(x)).reduce(_ + ", " + _)
          if(!givenIndirectMappingLecture) {
            log.warn("The remapping "+name+desc+" -> "+target+" will be indirectly propergated through external dependencies! "+
                     "This is extermely unsafe, and MOST LIKELY AN ERROR!!")
            log.warn("A method in one of your classes must be renamed to match a method it overrides or implements. "+
                     "Another of its superclasses declares an unrelated method with the same name and signature, "+
                     "which must be renamed as well to prevent errors.")
            log.warn("Unfortunately, one of these classes exists inside an external dependency. "+
                     "As indirect mapping can cause previously unmapped methods to be mapped through the addition of a new class, "+
                     "this remapping could very well not exist in the external dependency!")
            log.warn("Ignore this warning only if you know exactly what you are doing!")
            log.warn(externalClassLine)
            log.warn("")
            givenIndirectMappingLecture = true
          } else
            log.warn("The remapping "+name+desc+" -> "+target+" will be indirectly propergated through external dependencies! "+
                     "This is extermely unsafe, and MOST LIKELY AN ERROR!! " + externalClassLine)
        }

        if(directClasses.size > 0)
          log.debug("Propergating remapping "+name+desc+" -> "+target+" from "+prettySeq(sourceClasses)+" to "+prettySeq(directClasses)+".")
        if(indirectClasses.size > 0)
          log.debug("Indirectly propergating remapping "+name+desc+" -> "+target+" from "+prettySeq(sourceClasses)+" to "+prettySeq(indirectClasses)+".")

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
      override def mapInvokeDynamicMethodName(name: String, desc: String) =
        sys.error("Uh, you know that you're supposed to write mods targeting Java 6, right?")
    }
    (mapping, targetJar.mapWithVisitor(cv => new RemappingClassAdapter(cv, mapper)))
  }
}
