package moe.lymia.sbt.forge

import sbt._

import java.io._
import java.util.zip._
import java.util.jar._

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap, ArrayBuffer}

import language._

object asm {
  // TODO: Make method name/method desc easier to work with
  case class MethodName(name: String, desc: String)
  case class FieldName (name: String, desc: String)
  private class MapWrapperSeq[A, B](map: collection.mutable.Map[A, B]) extends Seq[B] {
    def iterator = map.iterator.map(_._2)
    def apply(i: Int) = iterator.drop(i).next
    def length = map.size
  }
  trait AnnotationContainer {
    def visibleAnnotations  : collection.mutable.Buffer[AnnotationNode]
    def invisibleAnnotations: collection.mutable.Buffer[AnnotationNode]
  }
  implicit class RichMethodNode(mn: MethodNode) extends AnnotationContainer {
    def methodName = MethodName(mn.name, mn.desc)

    def visibleAnnotations = {
      if(mn.visibleAnnotations == null) mn.visibleAnnotations = new ArrayBuffer[AnnotationNode]
      mn.visibleAnnotations
    }
    def invisibleAnnotations = {
      if(mn.invisibleAnnotations == null) mn.invisibleAnnotations = new ArrayBuffer[AnnotationNode]
      mn.invisibleAnnotations
    }
  }
  implicit class RichFieldNode(fn: FieldNode) extends AnnotationContainer {
    def fieldName = FieldName(fn.name, fn.desc)

    def visibleAnnotations = {
      if(fn.visibleAnnotations == null) fn.visibleAnnotations = new ArrayBuffer[AnnotationNode]
      fn.visibleAnnotations
    }
    def invisibleAnnotations = {
      if(fn.invisibleAnnotations == null) fn.invisibleAnnotations = new ArrayBuffer[AnnotationNode]
      fn.invisibleAnnotations
    }
  }
  implicit class RichClassNode(cn: ClassNode) extends AnnotationContainer {
    def visibleAnnotations = {
      if(cn.visibleAnnotations == null) cn.visibleAnnotations = new ArrayBuffer[AnnotationNode]
      cn.visibleAnnotations
    }
    def invisibleAnnotations = {
      if(cn.invisibleAnnotations == null) cn.invisibleAnnotations = new ArrayBuffer[AnnotationNode]
      cn.invisibleAnnotations
    }
  }
  class ClassNodeWrapper(private var inNode: ClassNode = null,
                         noCopy: Boolean = false) extends AnnotationContainer {
    val classNode = if(noCopy && inNode != null) inNode else {
      val cn = new ClassNode()
      if(inNode != null) inNode.accept(cn)
      cn
    }
    inNode = null // remove reference so we don't cause big leaks

    // TODO: Only do the data transformation when methodMap or fieldMap is requested

    // XXX: This might turn out to be unsafe?
    // TreeMap would be a little safer since ordering is a lot more guaranteed with those.
    // ... but scala.collection.mutable.TreeMap does not exist.
    val methodMap = new HashMap[MethodName, MethodNode]
    def addMethod(n: MethodNode) = methodMap.put(n.methodName, n)
    for(node <- classNode.methods) addMethod(node)
    classNode.methods = new MapWrapperSeq(methodMap)

    val fieldMap  = new HashMap[FieldName, FieldNode]
    def addField(n: FieldNode) = fieldMap.put(n.fieldName, n)
    for(node <- classNode.fields) addField(node)
    classNode.fields = new MapWrapperSeq(fieldMap)

    if(classNode.visibleAnnotations == null) classNode.visibleAnnotations = new ArrayBuffer[AnnotationNode]
    if(classNode.invisibleAnnotations == null) classNode.invisibleAnnotations = new ArrayBuffer[AnnotationNode]

    def visibleAnnotations   = classNode.visibleAnnotations
    def invisibleAnnotations = classNode.invisibleAnnotations

    def syncNames() {
      val methods = methodMap.values.toSeq
      methodMap.clear()
      for(m <- classNode.methods) addMethod(m)

      val fields  = fieldMap.keys.toSeq
      fieldMap.clear()
      for(f <- classNode.fields) addField(f)
    }

    override def clone() = new ClassNodeWrapper(classNode)
  }
  implicit def classNodeWrapper2ClassNode(wrapper: ClassNodeWrapper) = wrapper.classNode

  class JarData(val resources: HashMap[String, Array[Byte]]      = new HashMap[String, Array[Byte]],
                val classes  : HashMap[String, ClassNodeWrapper] = new HashMap[String, ClassNodeWrapper]) {
    def syncClassNames = new JarData(resources.clone, {
      val classes = new HashMap[String, ClassNodeWrapper]
      for((_, cn) <- this.classes) 
        if(classes.contains(cn.name)) sys.error("Repeat class name: "+cn.name)
        else classes.put(cn.name, new ClassNodeWrapper(cn))
      classes
    })
    def mapWithVisitor(visitor: ClassVisitor => ClassVisitor) =
      new JarData(resources.clone(),
                  classes.map { t =>
                    val cn  = t._2
                    val ncn = new ClassNode()
                    cn.accept(visitor(ncn))
                    (cn.name, new ClassNodeWrapper(ncn, noCopy = true))
                  }).syncClassNames
    override def clone() = new JarData(resources.clone(), classes.clone())
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
  def loadJarFile(in: InputStream) = {
    val jarData = new JarData()
    var entry: JarEntry = null
    val jin = new JarInputStream(in, false)
    while({entry = jin.getNextJarEntry; entry != null}) {
      if(entry.getName.endsWith(".class")) {
        val cn = readClassNode(jin)
        if(jarData.classes.contains(cn.name)) sys.error("Repeat class name: "+cn.name)
        jarData.classes.put(cn.name, new ClassNodeWrapper(cn, noCopy = true))
      } else jarData.resources.put(entry.getName, IO.readBytes(jin))
    }
    jarData
  }
  def writeJarFile(idata: JarData, out: OutputStream) = {
    val data = idata.syncClassNames
    val jout = new JarOutputStream(out)
    for((name, data) <- data.resources) {
      jout.putNextEntry(new JarEntry(name))
      jout.write(data)
    }
    for((name, cn) <- data.classes) {
      jout.putNextEntry(new JarEntry(name+".class"))
      jout.write(dumpClassNode(cn))
    }
    jout.close()
  }
}
