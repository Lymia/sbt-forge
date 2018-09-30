package moe.lymia.forge.asm

import java.io.{File => _, _}
import java.util.jar.{Attributes, JarEntry, JarInputStream, JarOutputStream, Manifest}

import org.apache.commons.io.FilenameUtils
import org.objectweb.asm._
import org.objectweb.asm.tree._
import sbt._

import scala.collection.JavaConverters._
import scala.collection.mutable

sealed trait ClassData {
  def name: String
  def mapWithVisitor(visitor: ClassVisitor => ClassVisitor): ClassData
  def toBytes: Array[Byte]
}
object ClassData {
  case class Node(cn: ClassNodeWrapper) extends ClassData {
    override def name = cn.name
    override def mapWithVisitor(visitor: ClassVisitor => ClassVisitor) = {
      val ncn = new ClassNode()
      cn.accept(visitor(ncn))
      Node(new ClassNodeWrapper(ncn, noCopy = true))
    }
    override def toBytes = dumpClassNode(cn)
  }
  case class Bytes(name: String, data: Array[Byte]) extends ClassData {
    override def mapWithVisitor(visitor: ClassVisitor => ClassVisitor) = {
      val ncn = new ClassNode()
      new ClassReader(data).accept(visitor(ncn), 0)
      Node(new ClassNodeWrapper(ncn, noCopy = true))
    }
    override def toBytes = data
  }
}

final class JarData(var resources: mutable.HashMap[String, Array[Byte]] = new mutable.HashMap[String, Array[Byte]],
                    var classes  : mutable.HashMap[String, ClassData]   = new mutable.HashMap[String, ClassData],
                    var identity : String = "<unknown jar>",
                    var manifest : Manifest = new Manifest()) {
  private def syncClassNames = new JarData(resources.clone, {
    val classes = new mutable.HashMap[String, ClassData]
    for((_, cn) <- this.classes)
      if(classes.contains(cn.name)) sys.error(s"Duplicate class name: ${cn.name}")
      else classes.put(cn.name, cn)
    classes
  }, identity, manifest)
  def mapWithVisitor(visitor: ClassVisitor => ClassVisitor, appliesTo: String => Boolean = _ => true) =
    new JarData(resources.clone(),
                classes.map(x => if (appliesTo(x._1)) x.copy(_2 = x._2.mapWithVisitor(visitor)) else x),
                identity, manifest).syncClassNames

  def mergeWith(overriding: JarData, log: Logger = null, newIdentity: String = identity) =
    JarData.mergeAll(Seq(this, overriding), log, newIdentity)

  def stripSignatures =
    new JarData(resources.filter(x => !JarData.isSignatureFile(x._1)), classes, identity,
                JarData.stripSignaturesFromManifest(manifest))

  def getClass(name: String) = classes.get(name).map {
    case ClassData.Node(cn) => cn
    case ClassData.Bytes(byteName, data) =>
      val cn = new ClassNode()
      new ClassReader(data).accept(cn, 0)
      assert(cn.name == name && byteName == name)
      val node = new ClassNodeWrapper(cn, noCopy = true)
      classes.put(name, ClassData.Node(node))
      node
  }
  def getClassData(name: String) =
    classes.get(name).map(_.toBytes)
  def putClass(name: String, cn: ClassNode, noCopy: Boolean = false) =
    classes.put(name, ClassData.Node(new ClassNodeWrapper(cn, noCopy)))
  def putClass(name: String, cn: ClassNodeWrapper) =
    classes.put(name, ClassData.Node(cn))
  def putClass(name: String, data: Array[Byte]) =
    classes.put(name, ClassData.Bytes(name, data))

  def allClasses = classes.keysIterator.map(x => getClass(x).get)

  def write(out: OutputStream): Unit = {
    val synched = syncClassNames
    val jout = new JarOutputStream(out, synched.manifest)
    for((name, data) <- synched.resources) {
      jout.putNextEntry(new JarEntry(name))
      jout.write(data)
    }
    for((name, data) <- synched.classes) {
      jout.putNextEntry(new JarEntry(s"$name.class"))
      jout.write(data.toBytes)
    }
    jout.finish()
    jout.close()
  }
  def write(out: File): Unit = {
    val outStream = new FileOutputStream(out)
    try { write(outStream) } finally { outStream.close() }
  }
}
object JarData {
  private def isSignatureFile(file: String) =
    file.startsWith("META-INF/SIG-") || (
      file.startsWith("META-INF/") && (
        file.endsWith(".SF") || file.endsWith(".DSA") || file.endsWith(".RSA")))
  private def stripDigests(attributes: Attributes) =
    for (toRemove <- attributes.keySet().asScala.collect {
      case x: Attributes.Name if x.toString.endsWith("-Digest") || x.toString.contains("-Digest-") => x
    }) attributes.remove(toRemove)
  private def stripSignaturesFromManifest(origManifest: Manifest) = {
    val manifest = origManifest.deepCopy()
    stripDigests(manifest.getMainAttributes)
    val entries = manifest.getEntries
    for ((entry, attributes) <- entries.asScala) stripDigests(attributes)
    // TODO: Why do we still have leftover Name: entries?
    for ((toRemove, _) <- entries.asScala.filter(_._2.isEmpty)) entries.remove(toRemove)
    manifest
  }

  def mergeAll(jars: Seq[JarData], log: Logger = null, newIdentity: String = "<merged jar>") = {
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
      target.manifest = target.manifest.merge(jar.manifest)
    }
    target
  }

  def load(in: InputStream, identity: String = "<unknown jar>"): JarData = {
    val jarData = new JarData(identity = identity)
    var entry: JarEntry = null
    val jin = new JarInputStream(in, false)
    while ({entry = jin.getNextJarEntry; entry != null}) {
      val data = IO.readBytes(jin)
      if (entry.getName.endsWith(".class")) {
        val name = FilenameUtils.removeExtension(entry.getName)
        if (jarData.classes.contains(name)) sys.error(s"Duplicate class name: $name")
        jarData.classes.put(name, ClassData.Bytes(name, data))
      } else jarData.resources.put(entry.getName, data)
    }
    jarData.manifest = jin.getManifest
    jarData
  }
  def load(in: File): JarData = {
    val inStream = new FileInputStream(in)
    try { load(inStream, in.getName) } finally { inStream.close() }
  }
}

private[asm] trait JarImplicits {
  implicit class ManifestExt(mf: Manifest) {
    def merge(other: Manifest) = {
      val newManifest = deepCopy()
      newManifest.getMainAttributes.putAll(other.getMainAttributes : java.util.Map[_, _])
      for ((name, attributes) <- other.getEntries.asScala)
        if (newManifest.getEntries.containsKey(name))
          newManifest.getEntries.get(name).putAll(attributes : java.util.Map[_, _])
        else newManifest.getEntries.put(name, attributes.clone().asInstanceOf[Attributes])
      newManifest
    }
    def deepCopy() = {
      val newManifest = new Manifest()
      newManifest.getMainAttributes.putAll(mf.getMainAttributes : java.util.Map[_, _])
      for ((name, attributes) <- mf.getEntries.asScala)
        newManifest.getEntries.put(name, attributes.clone().asInstanceOf[Attributes])
      newManifest
    }
  }
}