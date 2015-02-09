package moe.lymia.sbt.forge

import sbt._

import java.io._
import java.util.zip._
import java.util.jar._
import java.util.Arrays

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap, ArrayBuffer, HashSet}

object asm {
  case class JarData(resources: HashMap[String, Array[Byte]] = new HashMap[String, Array[Byte]],
                     classes  : HashMap[String, ClassNode]   = new HashMap[String, ClassNode]) {
    def syncClassNames = JarData(resources.clone, {
      val classes = new HashMap[String, ClassNode]
      for((_, cn) <- this.classes) 
        if(classes.contains(cn.name)) sys.error("Repeat class name: "+cn.name)
        else classes.put(cn.name, cn)
      classes
    })
  }
  def loadJarFile(in: InputStream) = {
    val jarData = new JarData()
    var entry: JarEntry = null
    val jin = new JarInputStream(in, false)
    while({entry = jin.getNextJarEntry; entry != null}) {
      if(entry.getName.endsWith(".class")) {
        val cr = new ClassReader(jin)
        val cn = new ClassNode()
        cr.accept(cn, 0)
        if(jarData.classes.contains(cn.name)) sys.error("Repeat class name: "+cn.name)
        jarData.classes.put(cn.name, cn)
      } else jarData.resources.put(entry.getName, IO.readBytes(jin))
    }
    jarData
  }
  def writeJarFile(data: JarData, out: OutputStream) = {
    val written = new HashSet[String]
    val jout = new JarOutputStream(out)
    for((name, data) <- data.resources) {
      jout.putNextEntry(new JarEntry(name))
      jout.write(data)
    }
    for((name, cn) <- data.classes) {
      jout.putNextEntry(new JarEntry(name+".class"))
      val cw = new ClassWriter(ASM4)
      cn.accept(cw)
      jout.write(cw.toByteArray)
    }
    jout.close()
  }

  private def clone(cn: ClassNode) = {
    val n = new ClassNode()
    cn.accept(n)
    n
  }

  private def markSideOnly(o: java.util.List[AnnotationNode], side: String) = 
    if(!o.exists(_.desc == "Lcpw/mods/fml/relauncher/SideOnly;")){
      val an = new AnnotationNode("Lcpw/mods/fml/relauncher/SideOnly;")
      an.visitEnum("value", "Lcpw/mods/fml/relauncher/Side;", side)
      o += an
    }
  private def markSideOnly(o: ClassNode, side: String) {
    if(o.visibleAnnotations == null) o.visibleAnnotations = new ArrayBuffer[AnnotationNode]
    markSideOnly(o.visibleAnnotations, side)
  }
  private def markSideOnly(o: MethodNode, side: String) {
    if(o.visibleAnnotations == null) o.visibleAnnotations = new ArrayBuffer[AnnotationNode]
    markSideOnly(o.visibleAnnotations, side)
  }
  private def markSideOnly(o: FieldNode, side: String) {
    if(o.visibleAnnotations == null) o.visibleAnnotations = new ArrayBuffer[AnnotationNode]
    markSideOnly(o.visibleAnnotations, side)
  }

  def buildMethodMap(f: java.util.List[MethodNode]): Map[(String, String), MethodNode] =
    if(f == null) Map()
    else f.map(x => (x.name, x.desc) -> x).toMap
  def buildFieldMap(f: java.util.List[FieldNode]): Map[(String, String), FieldNode] =
    if(f == null) Map()
    else f.map(x => (x.name, x.desc) -> x).toMap

  def merge(client: JarData, server: JarData, forge: JarData, log: Logger) = {
    val target = new JarData()
    for((name, data) <- client.resources)
      target.resources.put(name, data)
    for((name, data) <- server.resources) target.resources.get(name) match {
      case Some(cdata) => if(!Arrays.equals(cdata, data)) sys.error("Resource "+name+" does not match between client and server.")
      case None => target.resources.put(name, data)
    }
    for((name, data) <- forge.resources) {
      if(target.resources.contains(name) && !Arrays.equals(target.resources(name), data))
        log.warn("Forge overrides resource "+name+" with differing file.")
      target.resources.put(name, data)
    }

    for((name, cn) <- client.classes) {
      val ncn = clone(cn)
      if(!server.classes.contains(name)) markSideOnly(ncn, "CLIENT")
      target.classes.put(name, ncn)
    }
    for((name, serverClass) <- server.classes) {
      target.classes.get(name) match {
        case Some(clientClass) =>
          // diff fields
          val serverFields = buildFieldMap(serverClass.fields)
          val clientFields = buildFieldMap(clientClass.fields)

          for(t <- clientFields.keySet -- serverFields.keySet)
            markSideOnly(clientFields(t), "CLIENT")
          for(t <- serverFields.keySet -- clientFields.keySet) {
            markSideOnly(serverFields(t), "SERVER")
            clientClass.fields.add(serverFields(t))
          }

          // diff methods
          val serverMethods = buildMethodMap(serverClass.methods)
          val clientMethods = buildMethodMap(clientClass.methods)

          for(t <- clientMethods.keySet -- serverMethods.keySet)
            markSideOnly(clientMethods(t), "CLIENT")
          for(t <- serverMethods.keySet -- clientMethods.keySet) {
            markSideOnly(serverMethods(t), "SERVER")
            clientClass.methods.add(serverMethods(t))
          }
        case None =>
          val ncn = clone(serverClass)
          markSideOnly(ncn, "SERVER")
          target.classes.put(name, ncn)
      }
    }
    for((name, cn) <- forge.classes) {
      if(target.classes.contains(name)) log.warn("Forge jar overrides class "+name+" in Minecraft binaries.")
      target.classes.put(name, cn)
    }
    target
  }
}
