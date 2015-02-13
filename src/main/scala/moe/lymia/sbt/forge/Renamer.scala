package moe.lymia.sbt.forge

import sbt._
import asm._
import mapping._

import java.io._
import java.util.jar._

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, MultiMap}

object Renamer {
  def isSystemClass(name: String) = 
    try {
      ClassLoader.getSystemClassLoader().loadClass(name.replace("/", "."))
      true
    } catch {
      case _: Throwable => false
    }

  val classFileRegex = "([^.]+)\\.class".r
  private def findClassesInDirectory(path: String, file: File): Seq[(String, File)] = 
    if(file.isDirectory) {
      file.listFiles.flatMap { f =>
        val npath = (if(path == "") f.getName else path+"/"+f.getName)
        findClassesInDirectory(npath, f)
      }
    } else path match {
      case classFileRegex(name) => Seq((name, file))
      case _ => Seq()
    }
  class ClasspathSearcher(targetJar: JarData, classPath: Seq[File], log: Logger) {
    private val classCache    = new HashMap[String, ClassNodeWrapper]
    private val classSources  = new HashMap[String, () => Array[Byte]]
    private val classLocation = new HashMap[String, File]
    def resolve(name: String): Option[ClassNodeWrapper] = 
      targetJar.classes.get(name) orElse classCache.get(name) orElse classSources.get(name).map { x =>
        val cr = new ClassReader(new ByteArrayInputStream(x()))
        val cn = new ClassNode()
        cr.accept(cn, ClassReader.EXPAND_FRAMES)
        val cw = new ClassNodeWrapper(cn, noCopy = true)
        classCache.put(name, cw)
        cw
      }
    def classLocation(name: String): String = 
      targetJar.classes.get(name).map(_ => "<target jar>") orElse
      classLocation.get(name).map(_.getName) getOrElse
      (if(isSystemClass(name)) "<system classloader>" else "<unknown>")

    for(path <- classPath) {
      log.debug("Indexing classes in "+path)
      if(path.isDirectory)
        for((name, file) <- findClassesInDirectory("", path))
          if(targetJar.classes.contains(name)) log.warn(path+" defines class "+name+" already defined in target jar!")
          else if(!classSources.contains(name)) {
            classSources.put(name, () => IO.readBytes(file))
            classLocation.put(name, path)
          } else log.warn(path+" defines class "+name+" already defined in "+classSources(name)+"!")
      else {
        val jarFile = new JarFile(path)
        for(entry <- jarFile.entries()) entry.getName match {
          case classFileRegex(name) =>
            if(targetJar.classes.contains(name)) log.warn(path+" defines class "+name+" already defined in target jar!")
            else if(!classSources.contains(name)) {
              classSources.put(name, () => IO.readBytes(new URL("jar:"+path.toURI.toURL+"!/"+entry.getName).openStream()))
              classLocation.put(name, path)
            } else log.warn(path+" defines class "+name+" already defined in "+classSources(name)+"!")
          case _ =>
        }
        jarFile.close()
      }
    }
  }

  val splitNameRegex = """^(.*)\$([^$]+)$""".r
  def findRemappableInnerClass(targetJar: JarData, mapping: ForgeMapping, log: Logger) {
    val classMapping = mapping.classMapping
    for((name, node) <- targetJar.classes) {
      val outerClass = if(node.outerClass != null) node.outerClass
                       else if(!name.contains("$")) null
                       else name match {
                         case splitNameRegex(outer, _) =>
                           node.outerClass = outer
                           outer
                         case _ => null
                       }
      if(outerClass != null &&
         !classMapping.contains(name) && classMapping.contains(outerClass) &&
         name.startsWith(outerClass+"$")) {
        val newName = classMapping(outerClass) + name.substring(outerClass.length)
        log.debug("Adding mapping for inner class "+name+" to "+newName)
        classMapping.put(name, newName)
      }
    }
  }
  def buildSuperclassMap(seeds: Seq[String], searcher: ClasspathSearcher, log: Logger) = {
    val map = new HashMap[String, Option[Set[String]]]
    def recurseClass(name: String): Option[Set[String]] = map.get(name) match {
      case Some(x) => x
      case None =>
        searcher.resolve(name) match {
          case Some(node) =>
            val seq = Some(Set(node.superName) ++ node.interfaces ++
                           recurseClass(node.superName).getOrElse(Set()) ++ 
                           (node.interfaces map (recurseClass _) flatMap (_.getOrElse(Set()))))
            map.put(name, seq)
            seq
          case None => 
            if(!isSystemClass(name)) log.warn("Could not resolve class "+name+"!")
            None
        }
    }
    for(name <- seeds) recurseClass(name)
    map.flatMap(x => x._2.map(v => Map(x._1 -> v)).getOrElse(Map())).toMap
  }
  def findRenamingCandidates(searcher: ClasspathSearcher, mapping: ForgeMapping,
                             classList: Seq[String], log: Logger) = {
    val couldRename = mapping.methodMapping.keys.map(x => MethodName(x.name, x.desc)).toSet    
    val renameCandidates = new HashMap[MethodName, mutable.Set[String]] with MultiMap[MethodName, String]
    for(name <- classList) {
      val cn = searcher.resolve(name) getOrElse sys.error("Class "+name+" disappeared??")
      for((name, mn) <- cn.methodMap)
        if(couldRename.contains(name))
          if((mn.access & ACC_STATIC) != ACC_STATIC && (mn.access & ACC_PRIVATE) != ACC_PRIVATE)
            renameCandidates.addBinding(name, cn.name)
          else log.debug("Method "+cn.name+"."+name.name+name.desc+" is private or static, and will not be "+
                         "considered for propergation.")
    }
    renameCandidates.toMap.mapValues(_.toSet)
  }
  private def prettySeq[T](s: Seq[T]) = 
    if(s.size == 0) "<nothing>"
    else if(s.length == 1) s.head.toString
    else "["+s.map(_.toString).reduce(_ + "," + _)+"]"
  private def prettySeq[T](s: Set[T]): String =
    prettySeq(s.toSeq)
  def buildEquivalenceSets(methodName: String, methodDesc: String, candidates: Set[String], 
                           superclassMap: Map[String, Set[String]],
                           mapping      : ForgeMapping, log: Logger) = {
    def isSuperclass(name: String, target: String) = superclassMap(name).contains(target)
    def isSubclass  (name: String, target: String) = superclassMap(target).contains(name)
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

    val noDirectMapping = mappingStep(nonMapped.toSeq, isSubclass)
    @annotation.tailrec def mapRecursive(candidates: Seq[String]): Seq[String] = {
      val remaining = mappingStep(candidates, (a, b) => isSuperclass(a, b) || isSubclass(a, b))
      if(remaining == candidates || remaining.length == 0) remaining
      else mapRecursive(remaining)
    }
    val remaining = mapRecursive(noDirectMapping)

    (nonMapped.toSet, remaining.toSet, noDirectMapping.toSet, equivalenceMap.toMap.mapValues(_.toSet))
  }
  def applyMapping(targetJar: JarData, classPath: Seq[File], imapping: ForgeMapping, log: Logger,
                   fixInnerClasses: Boolean = false) = {
    val mapping = imapping.clone()

    if(fixInnerClasses) {
      log.info("Mapping inner class names...")
      if(mapping.classMapping.size > 0 || mapping.packageMapping.size > 0)
        findRemappableInnerClass(targetJar, mapping, log)
    }

    log.info("Indexing dependency jars...")
    val searcher = new ClasspathSearcher(targetJar, classPath, log)

    log.info("Building super/subclass maps...")
    val superclassMap    = buildSuperclassMap(targetJar.classes.keys.toSeq, searcher, log)

    log.info("Propergating mapping...")
    val renameCandidates = findRenamingCandidates(searcher, mapping, superclassMap.keys.toSeq, log)
    var givenIndirectMappingLecture = false
    renameCandidates.foreach { t =>
      val (MethodName(name, desc), candidates) = t
      val (noMapping, remaining, noDirectMapping, equivalenceMap) = 
        buildEquivalenceSets(name, desc, candidates, superclassMap, mapping, log) 
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
          log.debug("Propergating remapping "+name+desc+" -> "+target+" from "+prettySeq(directClasses)+" to "+prettySeq(directClasses)+".")
        if(indirectClasses.size > 0)
          log.debug("Indirectly propergating remapping "+name+desc+" -> "+target+" from "+prettySeq(directClasses)+" to "+prettySeq(indirectClasses)+".")

        for(n <- newMappings) mapping.methodMapping.put(MethodSpec(n, name, desc), target)
      }
    }

    log.info("Mapping classes...")
    targetJar.mapWithVisitor(mapping.visitor _)
  }
}
