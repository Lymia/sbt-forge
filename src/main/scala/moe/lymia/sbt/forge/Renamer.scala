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
      classLocation.get(name).map(_.toString) getOrElse
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

  def findRemappableInnerClass(targetJar: JarData, mapping: ForgeMapping, log: Logger) {
    val classMapping = mapping.classMapping
    for((name, node) <- targetJar.classes)
      if(node.outerClass != null &&
         !classMapping.contains(name) && classMapping.contains(node.outerClass) &&
         name.startsWith(node.outerClass+"$")) {
        val newName = classMapping(node.outerClass) + name.substring(node.outerClass.length)
        log.debug("Adding mapping for inner class "+name+" to "+newName)
        classMapping.put(name, newName)
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
  // TODO: Treat private classes different
  def findRenamingCandidates(searcher: ClasspathSearcher, mapping: ForgeMapping,
                             classList: Seq[String], log: Logger) = {
    val couldRename = mapping.methodMapping.keys.map(x => MethodName(x.name, x.desc)).toSet    
    val renameCandidates = new HashMap[MethodName, mutable.Set[String]] with MultiMap[MethodName, String]
    for(name <- classList) {
      val cn = searcher.resolve(name) getOrElse sys.error("Class "+name+" disappeared??")
      for((name, mn) <- cn.methodMap)
        if(couldRename.contains(name))
          if((mapping.methodMapping.contains(MethodSpec(cn.name, name.name, name.desc)) ||
             !((mn.access & ACC_STATIC) == ACC_STATIC || (mn.access & ACC_PRIVATE) == ACC_PRIVATE)))
            renameCandidates.addBinding(name, cn.name)
          else log.debug("Method "+cn.name+"."+name.name+name.desc+" is private or static, and will not be "+
                         "considered for propergation.")
    }
    renameCandidates.map(x => (x._1, x._2.toSet)).toMap
  }

  private def prettySeq[T](s: Seq[T]) = 
    if(s.size == 0) "<nothing>"
    else if(s.length == 1) s.head.toString
    else "["+s.map(_.toString).reduce(_ + "," + _)+"]"
  def buildEquivalenceSets(methodName: String, methodDesc: String,
                           seeds: Seq[String], candidates: Set[String], 
                           superclassMap: Map[String, Set[String]],
                           mapping      : ForgeMapping, log: Logger) = {
    def isSuperclass(name: String, target: String) = superclassMap(name).contains(target)
    def isSubclass  (name: String, target: String) = superclassMap(target).contains(name)
    def recurse(sets: Map[String, Set[String]], remaining: Seq[String]): Map[String, Set[String]] = {
      val (satisified, left) = remaining.map{ name =>
        (name, sets.filter(x => x._2.exists(target => isSuperclass(name, target) || isSubclass(name, target))).map(_._1).toSet)
      }.partition(x => x._2.size > 0)

      val nsets = satisified.foldLeft(sets){(sets, v) => 
        val (name, matchedSets) = v
        val (toMerge, left) = sets.partition(x => matchedSets.intersect(x._2).size > 0)
        val sourceMappings = toMerge.map(x => mapping.methodMapping(MethodSpec(x._1, methodName, methodDesc))).toSet
        if(sourceMappings.size > 1) {
          log.error("Mapping conflict while finding methods related to "+methodName+methodDesc)
          log.error("  Currently processing class: "+name)
          log.error("  Conflicting target names: "+sourceMappings.reduce(_ + ", " + _))
          log.error("  Involving the following mapping sets:")
          for((name, set) <- sets) {
            val resolve = mapping.methodMapping(MethodSpec(name, methodName, methodDesc))
            log.error("    "+name+"."+methodName+methodDesc+" -> "+resolve+": "+set.reduce(_ + ", " + _))
          }
          sys.error("Mapping conflict found!")
        } else Map(toMerge.head._1 -> (toMerge.map(_._2).reduce(_ ++ _) + name)) ++ left
      }

      if(left.length == 0 || satisified.length == 0) {
        if(left.length > 0) log.debug("Classes have method "+methodName+methodDesc+
                                      ", but no mapping found: "+prettySeq(left.map(_._1)))
        nsets
      } else recurse(nsets, left.map(_._1))
    }
    recurse(seeds.map(x => x -> Set(x)).toMap, (candidates -- seeds).toSeq)
  }
  def applyMapping(targetJar: JarData, classPath: Seq[File], imapping: ForgeMapping, log: Logger) = {
    log.info("Mapping class names...")
    val mapping = imapping.clone()
    if(mapping.classMapping.size > 0 || mapping.packageMapping.size > 0)
      findRemappableInnerClass(targetJar, mapping, log)

    log.info("Indexing dependency jars...")
    val searcher = new ClasspathSearcher(targetJar, classPath, log)

    log.info("Building super/subclass maps...")
    val superclassMap    = buildSuperclassMap(targetJar.classes.keys.toSeq, searcher, log)

    log.info("Propergating mapping...")
    val renameCandidates = findRenamingCandidates(searcher, mapping, superclassMap.keys.toSeq, log)
    renameCandidates.foreach { t =>
      val (MethodName(name, desc), candidates) = t
      val (seeds, otherCandidates) = candidates.partition(n =>
        mapping.methodMapping.contains(MethodSpec(n, name, desc)))
      buildEquivalenceSets(name, desc, seeds.toSeq, otherCandidates,
                           superclassMap, mapping, log) foreach { t =>
        val (seed, set) = t
        val target = mapping.methodMapping(MethodSpec(seed, name, desc))
        val (addTo, left) = set.partition(x => !mapping.methodMapping.contains(MethodSpec(x, name, desc)))
        if(addTo.size != 0) log.debug("Propergating remapping "+name+desc+" -> "+target+" from "+
                                      prettySeq(left.toSeq)+" to "+prettySeq(addTo.toSeq)+".")
        val externalProp = addTo.filter(x => !targetJar.classes.contains(x))
        if(externalProp.size > 0) log.warn("The remapping "+name+desc+" -> "+target+" was propergated to the following "+
                                           "external dependencies. They must be deobfusicated with a similar mapper, or "+
                                           "else bad things will happen: "+
                                           externalProp.map(x => x + " in " + searcher.classLocation(x)).reduce(_ + ", " + _))
        addTo foreach { n =>
          mapping.methodMapping.put(MethodSpec(n, name, desc), target)
        }
      }
    }

    log.info("Mapping classes...")
    targetJar.mapWithVisitor(mapping.visitor _)
  }
}
