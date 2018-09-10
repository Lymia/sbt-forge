package moe.lymia.sbt.forge

import sbt._
import java.io._
import java.util

import org.objectweb.asm.tree._
import asm._

import scala.collection.JavaConverters._

// Merges the client and server sides, already patched using binpatches.jar
// As this applies @SideOnly implicitly, we don't worry about that here.
object Merger {
  val SideClassName = "net/minecraftforge/fml/relauncher/Side"
  val SideOnlyClassName = "net/minecraftforge/fml/relauncher/SideOnly"

  private case class InnerClassData(innerName: String, name: String, outerName: String)
  private object InnerClassData {
    def apply(icn: InnerClassNode): InnerClassData = apply(icn.innerName, icn.name, icn.outerName)
  }
  def merge(clientPath: File, serverPath: File, classesPath: File, serverDepPrefixes: Seq[String],
            log: Logger) = {
    val client = loadJarFile(new FileInputStream(clientPath))
    val server = loadJarFile(new FileInputStream(serverPath))
    val classes = loadJarFile(new FileInputStream(classesPath))
    val target = new JarData()

    for((name, data) <- client.resources)
      target.resources.put(name, data)
    for((name, data) <- server.resources) target.resources.get(name) match {
      case Some(cdata) =>
        if(!util.Arrays.equals(cdata, data))
          sys.error(s"Resource $name does not match between client and server.")
      case None =>
        if (!serverDepPrefixes.exists(x => name.startsWith(x)))
          target.resources.put(name, data)
    }

    for((name, clientClass) <- client.classes) {
      log.debug(s"Copying class $name from client.")
      target.classes.put(name, clientClass)
    }
    for ((name, serverClass) <- server.classes) {
      target.classes.get(name) match {
        case Some(clientClass) =>
          log.debug(s"Merging class $name between server and client.")

          // Merge methods
          for ((name, method) <- serverClass.methodMap)
            if (!clientClass.methodMap.contains(name))
              clientClass.methodMap.put(name, method)

          // Merge fields
          for ((name, field) <- serverClass.fieldMap)
            if (!clientClass.fieldMap.contains(name))
              clientClass.fieldMap.put(name, field)

          // Merge inner classes
          val clientInnerClasses = clientClass.innerClasses.asScala.map(x => InnerClassData(x)).toSet
          for (serverInnerClass <- serverClass.innerClasses.asScala)
            if (!clientInnerClasses.contains(InnerClassData(serverInnerClass)))
              clientClass.innerClasses.add(serverInnerClass)
        case None =>
          if (!serverDepPrefixes.exists(x => name.startsWith(x))) {
            log.debug(s"Copying class $name from server.")
            target.classes.put(name, serverClass)
          }
      }
    }

    target.classes.put(SideClassName,
                       classes.classes.getOrElse(SideClassName, sys.error("Side not found in classes.jar")))
    target.classes.put(SideOnlyClassName,
                       classes.classes.getOrElse(SideOnlyClassName, sys.error("SideOnly not found in classes.jar")))

    target
  }
  def addForgeClasses(minecraft: JarData, forge: JarData, log: Logger) = {
    val target = new JarData()

    for((name, data) <- minecraft.resources) target.resources.put(name, data)
    for((name, data) <- forge.resources) {
      if(target.resources.contains(name) && !util.Arrays.equals(target.resources(name), data))
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
