package moe.lymia.forge.build

import java.util

import moe.lymia.forge.asm._
import org.objectweb.asm.tree._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable

object Merger {
  val SideClassName = "net/minecraftforge/fml/relauncher/Side"
  val SideOnlyClassName = "net/minecraftforge/fml/relauncher/SideOnly"

  private def markSideOnly[T: AnnotationContainer](c: T, side: String): Unit = {
    if(!c.visibleAnnotations.exists(_.desc == s"L$SideOnlyClassName;")){
      val an = new AnnotationNode(s"L$SideOnlyClassName;")
      an.visitEnum("value", s"L$SideClassName;", side)
      c.visibleAnnotations += an
    }
  }
  private def mergeLists[A, B: AnnotationContainer](client: mutable.LinkedHashMap[A, B],
                                                    server: mutable.LinkedHashMap[A, B]): Unit = {
    for ((name, value) <- client)
      if (!server.contains(name))
        markSideOnly(value, "CLIENT")
    for ((name, value) <- server)
      if (!client.contains(name)) {
        markSideOnly(value, "SERVER")
        client.put(name, value)
      }
  }

  private case class InnerClassData(innerName: String, name: String, outerName: String)
  private object InnerClassData {
    def apply(icn: InnerClassNode): InnerClassData = apply(icn.innerName, icn.name, icn.outerName)
  }
  def merge(clientPath: File, serverPath: File, log: Logger) = {
    val client = JarData.load(clientPath).stripSignatures
    val server = JarData.load(serverPath).stripSignatures
    val target = new JarData()

    for((name, data) <- client.resources)
      target.resources.put(name, data)
    for((name, data) <- server.resources) target.resources.get(name) match {
      case Some(cdata) =>
        if(!util.Arrays.equals(cdata, data))
          sys.error(s"Resource $name does not match between client and server.")
      case None =>
        target.resources.put(name, data)
    }

    for (clientClass <- client.allClasses) {
      log.debug(s"Copying class ${clientClass.name} from client.")
      target.putClass(clientClass.name, clientClass)

      if (!server.classes.contains(clientClass.name))
        markSideOnly(clientClass, "CLIENT")
    }
    for (serverClass <- server.allClasses) {
      target.getClass(serverClass.name) match {
        case Some(clientClass) =>
          log.debug(s"Merging class ${serverClass.name} between server and client.")

          mergeLists(clientClass.methodMap, serverClass.methodMap)
          mergeLists(clientClass.fieldMap, serverClass.fieldMap)

          // Merge inner classes
          val clientInnerClasses = clientClass.innerClasses.asScala.map(x => InnerClassData(x)).toSet
          for (serverInnerClass <- serverClass.innerClasses.asScala)
            if (!clientInnerClasses.contains(InnerClassData(serverInnerClass)))
              clientClass.innerClasses.add(serverInnerClass)
        case None =>
          log.debug(s"Copying class ${serverClass.name} from server.")
          target.putClass(serverClass.name, serverClass)
          markSideOnly(serverClass, "SERVER")
      }
    }

    target.manifest = client.manifest.merge(server.manifest)
    target
  }
}
