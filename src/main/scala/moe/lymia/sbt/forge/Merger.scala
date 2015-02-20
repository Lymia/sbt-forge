package moe.lymia.sbt.forge

import sbt._

import java.io._
import java.util.Arrays

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

import asm._

object Merger {
  private def markSideOnly(c: AnnotationContainer, side: String, classesMoved: Boolean): Unit = {
    val pkg = if(classesMoved) "net/minecraftforge/fml/relauncher" else "cpw/mods/fml/relauncher"
    if(!c.visibleAnnotations.exists(_.desc == "L"+pkg+"/SideOnly;")){
      val an = new AnnotationNode("L"+pkg+"/SideOnly;")
      an.visitEnum("value", "L"+pkg+"/Side;", side)
      c.visibleAnnotations += an
    }
  }

  def forgeGradleMerge(client: File, server: File, outJar: File, config: InputStream, classesMoved: Boolean) {
    new ForgeGradleMergeJars().mergeJars(client, server, outJar, config, classesMoved)
  }

  def merge(client: JarData, server: JarData, config: Seq[String], log: Logger, classesMoved: Boolean) = {
    val dontAnnotate = config.filter(_.startsWith("!")).map(_.substring(1)).toSet
    val exclude      = config.filter(_.startsWith("^")).map(_.substring(1))
    // copyToServer and copyToClient do nothing in ForgeGradle. Just ignore them for now.
    def isExcluded(name: String) = exclude.exists(name.startsWith _)

    val target = new JarData()
    for((name, data) <- client.resources) if(!isExcluded(name))
      target.resources.put(name, data)
    for((name, data) <- server.resources) if(!isExcluded(name)) target.resources.get(name) match {
      case Some(cdata) => if(!Arrays.equals(cdata, data)) sys.error("Resource "+name+" does not match between client and server.")
      case None => target.resources.put(name, data)
    }

    for((name, cn) <- client.classes) if(!isExcluded(name)) {
      val ncn = cn.clone()
      if(!dontAnnotate.contains(name)) if(!server.classes.contains(name)) markSideOnly(ncn, "CLIENT", classesMoved)
      target.classes.put(name, ncn)
    }
    for((name, serverClass) <- server.classes) if(!isExcluded(name)) {
      target.classes.get(name) match {
        case Some(clientClass) =>
          // diff fields
          for(t <- clientClass.fieldMap.keySet -- serverClass.fieldMap.keySet)
            markSideOnly(clientClass.fieldMap(t), "CLIENT", classesMoved)
          for(t <- serverClass.fieldMap.keySet -- clientClass.fieldMap.keySet) {
            val field = serverClass.fieldMap(t)
            markSideOnly(field, "SERVER", classesMoved)
            clientClass.addField(field)
          }

          // diff methods
          for(t <- clientClass.methodMap.keySet -- serverClass.methodMap.keySet)
            markSideOnly(clientClass.methodMap(t), "CLIENT", classesMoved)
          for(t <- serverClass.methodMap.keySet -- clientClass.methodMap.keySet) {
            val method = serverClass.methodMap(t)
            markSideOnly(method, "SERVER", classesMoved)
            clientClass.addMethod(method)
          }
        case None =>
          val ncn = serverClass.clone()
          if(!dontAnnotate.contains(name)) markSideOnly(ncn, "SERVER", classesMoved)
          target.classes.put(name, ncn)
      }
    }

    target
  }
  def addForgeClasses(minecraft: JarData, forge: JarData, log: Logger) = {
    val target = new JarData()

    for((name, data) <- minecraft.resources) target.resources.put(name, data)
    for((name, data) <- forge.resources) {
      if(target.resources.contains(name) && !Arrays.equals(target.resources(name), data))
        log.warn("Forge overrides resource "+name+" in Minecraft binaries.")
      target.resources.put(name, data)
    }

    for((name, cn) <- minecraft.classes) target.classes.put(name, cn.clone())
    for((name, cn) <- forge.classes) {
      if(target.classes.contains(name)) log.warn("Forge jar overrides class "+name+" in Minecraft binaries.")
      target.classes.put(name, cn)
    }

    target
  }
}
