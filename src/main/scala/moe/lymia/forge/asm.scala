package moe.lymia.forge

import java.io._
import java.util.ArrayList
import java.util.jar._

import org.objectweb.asm._
import org.objectweb.asm.tree._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.language._

object asm {
  private class MapWrapperSeq[A, B](map: mutable.LinkedHashMap[A, B]) extends Seq[B] {
    def iterator = map.valuesIterator
    def apply(i: Int) = map.valuesIterator.drop(i).next()
    def length = map.size
  }

  trait AnnotationContainer[T] {
    def visibleAnnotations  (t: T): mutable.Buffer[AnnotationNode]
    def invisibleAnnotations(t: T): mutable.Buffer[AnnotationNode]
  }
  implicit object MethodAnnotationContainer extends AnnotationContainer[MethodNode] {
    override def visibleAnnotations(mn: MethodNode) = {
      if (mn.visibleAnnotations == null) mn.visibleAnnotations = new ArrayList[AnnotationNode]
      mn.visibleAnnotations.asScala
    }
    override def invisibleAnnotations(mn: MethodNode) = {
      if (mn.invisibleAnnotations == null) mn.invisibleAnnotations = new ArrayList[AnnotationNode]
      mn.invisibleAnnotations.asScala
    }
  }
  implicit object FieldAnnotationContainer extends AnnotationContainer[FieldNode] {
    override def visibleAnnotations(fn: FieldNode) = {
      if (fn.visibleAnnotations == null) fn.visibleAnnotations = new ArrayList[AnnotationNode]
      fn.visibleAnnotations.asScala
    }
    override def invisibleAnnotations(fn: FieldNode) = {
      if (fn.invisibleAnnotations == null) fn.invisibleAnnotations = new ArrayList[AnnotationNode]
      fn.invisibleAnnotations.asScala
    }
  }
  implicit object ClassAnnotationContainer extends AnnotationContainer[ClassNodeWrapper] {
    override def visibleAnnotations(cn: ClassNodeWrapper) = {
      if (cn.classNode.visibleAnnotations == null)
        cn.classNode.visibleAnnotations = new ArrayList[AnnotationNode]
      cn.classNode.visibleAnnotations.asScala
    }
    override def invisibleAnnotations(cn: ClassNodeWrapper) = {
      if (cn.classNode.invisibleAnnotations == null)
        cn.classNode.invisibleAnnotations = new ArrayList[AnnotationNode]
      cn.classNode.invisibleAnnotations.asScala
    }
  }
  implicit class AnnotationContainerExt[T: AnnotationContainer](t: T) {
    def visibleAnnotations = implicitly[AnnotationContainer[T]].visibleAnnotations(t)
    def invisibleAnnotations = implicitly[AnnotationContainer[T]].invisibleAnnotations(t)
  }

  case class MethodName(name: String, desc: String)
  case class FieldName (name: String, desc: String)
  implicit class RichMethodNode(mn: MethodNode) {
    def methodName = MethodName(mn.name, mn.desc)
  }
  implicit class RichFieldNode(fn: FieldNode) {
    def fieldName = FieldName(fn.name, fn.desc)
  }
  class ClassNodeWrapper(private var inNode: ClassNode = null, noCopy: Boolean = false) {
    val classNode = if(noCopy && inNode != null) inNode else {
      val cn = new ClassNode()
      if(inNode != null) inNode.accept(cn)
      cn
    }
    inNode = null // remove reference so we don't cause big leaks

    val methodMap = new mutable.LinkedHashMap[MethodName, MethodNode]
    def addMethod(n: MethodNode) = methodMap.put(n.methodName, n)
    for(node <- classNode.methods.asScala) addMethod(node)
    classNode.methods = new MapWrapperSeq(methodMap).asJava

    val fieldMap  = new mutable.LinkedHashMap[FieldName, FieldNode]
    def addField(n: FieldNode) = fieldMap.put(n.fieldName, n)
    for(node <- classNode.fields.asScala) addField(node)
    classNode.fields = new MapWrapperSeq(fieldMap).asJava

    def syncNames() {
      val methods = methodMap.values.toSeq
      methodMap.clear()
      for(m <- methods) addMethod(m)

      val fields  = fieldMap.values.toSeq
      fieldMap.clear()
      for(f <- fields) addField(f)
    }

    override def clone() = new ClassNodeWrapper(classNode)
  }
  implicit def classNodeWrapper2ClassNode(wrapper: ClassNodeWrapper) = wrapper.classNode

  def mergeAllJars(newIdentity: String, jars: Seq[JarData], log: Logger = null) = {
    val target = new JarData(identity = newIdentity)
    val classLocation = new mutable.HashMap[String, String]
    val resourceLocation = new mutable.HashMap[String, String]
    for (jar <- jars) {
      for ((name, data) <- jar.classes) {
        if (log != null)
          for (loc <- classLocation.get(name)) log.warn(s"${jar.identity} overrides class $name in $loc")
        classLocation.put(name, jar.identity)
        target.classes.put(name, data)
      }
      for ((name, data) <- jar.resources) {
        if (log != null)
          for (loc <- resourceLocation.get(name)) log.warn(s"${jar.identity} overides resource $name in $loc")
        resourceLocation.put(name, jar.identity)
        target.resources.put(name, data)
      }
      target.manifest = mergeManifest(target.manifest, jar.manifest)
    }
    target
  }

  def mergeManifest(a: Manifest, b: Manifest) = {
    val newManifest = a.clone().asInstanceOf[Manifest]
    newManifest.getMainAttributes.putAll(b.getMainAttributes : java.util.Map[_, _])
    for ((name, attributes) <- b.getEntries.asScala)
      if (newManifest.getEntries.containsKey(name))
        newManifest.getEntries.get(name).putAll(attributes : java.util.Map[_, _])
      else newManifest.getEntries.put(name, attributes.clone().asInstanceOf[Attributes])
    newManifest
  }
  def cloneManifest(mf: Manifest) = {
    val newManifest = new Manifest()
    newManifest.getMainAttributes.putAll(mf.getMainAttributes : java.util.Map[_, _])
    for ((name, attributes) <- mf.getEntries.asScala)
      newManifest.getEntries.put(name, attributes.clone().asInstanceOf[Attributes])
    newManifest
  }

  private def isSignatureFile(file: String) =
    file.startsWith("META-INF/SIG-") || (
      file.startsWith("META-INF/") && (
        file.endsWith(".SF") || file.endsWith(".DSA") || file.endsWith(".RSA")))
  private def stripDigests(attributes: Attributes) =
    for (toRemove <- attributes.keySet().asScala.collect {
      case x: Attributes.Name if x.toString.endsWith("-Digest") || x.toString.contains("-Digest-") => x
    }) attributes.remove(toRemove)
  def stripSignaturesFromManifest(origManifest: Manifest) = {
    val manifest = cloneManifest(origManifest)
    stripDigests(manifest.getMainAttributes)
    val entries = manifest.getEntries
    for ((entry, attributes) <- entries.asScala) stripDigests(attributes)
    // TODO: Why do we still have leftover Name: entries?
    for ((toRemove, _) <- entries.asScala.filter(_._2.isEmpty)) entries.remove(toRemove)
    manifest
  }

  class JarData(var resources: HashMap[String, Array[Byte]]      = new HashMap[String, Array[Byte]],
                var classes  : HashMap[String, ClassNodeWrapper] = new HashMap[String, ClassNodeWrapper],
                var identity : String = "<unknown jar>",
                var manifest : Manifest = new Manifest()) {
    def syncClassNames = new JarData(resources.clone, {
      val classes = new mutable.HashMap[String, ClassNodeWrapper]
      for((_, cn) <- this.classes)
        if(classes.contains(cn.name)) sys.error(s"Duplicate class name: ${cn.name}")
        else classes.put(cn.name, new ClassNodeWrapper(cn))
      classes
    }, identity, manifest)
    def mapWithVisitor(visitor: ClassVisitor => ClassVisitor) =
      new JarData(resources.clone(),
                  classes.map { t =>
                    val cn  = t._2
                    val ncn = new ClassNode()
                    cn.accept(visitor(ncn))
                    (cn.name, new ClassNodeWrapper(ncn, noCopy = true))
                  }, identity, manifest).syncClassNames

    def mergeWith(overriding: JarData, log: Logger = null, newIdentity: String = identity) =
      mergeAllJars(newIdentity, Seq(this, overriding), log)

    def stripSignatures =
      new JarData(resources.filter(x => !isSignatureFile(x._1)), classes, identity,
                  stripSignaturesFromManifest(manifest))

    override def clone() =
      new JarData(resources.clone(), classes.clone(), identity, cloneManifest(manifest))
  }

  def readClassNode(in: InputStream) = {
    val cr = new ClassReader(in)
    val cn = new ClassNode()
    cr.accept(cn, ClassReader.EXPAND_FRAMES)
    cn
  }
  def dumpClassNode(cn: ClassNode) = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cn.accept(cw)
    cw.toByteArray
  }

  // TODO: Properly close streams here
  def loadJarFile(in: InputStream, identity: String = "<unknown jar>"): JarData = {
    val jarData = new JarData(identity = identity)
    var entry: JarEntry = null
    val jin = new JarInputStream(in, false)
    while({entry = jin.getNextJarEntry; entry != null}) {
      if(entry.getName.endsWith(".class")) {
        val cn = readClassNode(jin)
        if(jarData.classes.contains(cn.name)) sys.error(s"Duplicate class name: ${cn.name}")
        jarData.classes.put(cn.name, new ClassNodeWrapper(cn, noCopy = true))
      } else jarData.resources.put(entry.getName, IO.readBytes(jin))
    }
    jarData.manifest = jin.getManifest
    jarData
  }
  def loadJarFile(in: File): JarData =
    loadJarFile(new FileInputStream(in), in.getName)

  def writeJarFile(idata: JarData, out: OutputStream): Unit = {
    val data = idata.syncClassNames
    val jout = new JarOutputStream(out, data.manifest)
    for((name, data) <- data.resources) {
      jout.putNextEntry(new JarEntry(name))
      jout.write(data)
    }
    for((name, cn) <- data.classes) {
      jout.putNextEntry(new JarEntry(s"$name.class"))
      jout.write(dumpClassNode(cn))
    }
    jout.finish()
    jout.close()
  }
  def writeJarFile(idata: JarData, out: File): Unit =
    writeJarFile(idata, new FileOutputStream(out))
}
