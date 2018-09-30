package moe.lymia.forge.asm

import java.util.ArrayList

import org.objectweb.asm.tree.{AnnotationNode, ClassNode, FieldNode, MethodNode}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language._

private class MapWrapperSeq[A, B](map: mutable.LinkedHashMap[A, B]) extends Seq[B] {
  def iterator = map.valuesIterator
  def apply(i: Int) = map.valuesIterator.drop(i).next()
  def length = map.size
}

trait AnnotationContainer[T] {
  def visibleAnnotations  (t: T): mutable.Buffer[AnnotationNode]
  def invisibleAnnotations(t: T): mutable.Buffer[AnnotationNode]
}

case class MethodName(name: String, desc: String)
case class FieldName (name: String, desc: String)

final class ClassNodeWrapper(private var inNode: ClassNode = null, noCopy: Boolean = false) {
  val classNode = if(noCopy && inNode != null) inNode else {
    val cn = new ClassNode()
    if(inNode != null) inNode.accept(cn)
    cn
  }
  inNode = null // remove reference so we don't cause big leaks

  lazy val methodMap = {
    val map = new mutable.LinkedHashMap[MethodName, MethodNode]
    for(n <- classNode.methods.asScala) map.put(n.methodName, n)
    classNode.methods = new MapWrapperSeq(map).asJava
    map
  }
  lazy val fieldMap = {
    val map = new mutable.LinkedHashMap[FieldName, FieldNode]
    for(n <- classNode.fields.asScala) map.put(n.fieldName, n)
    classNode.fields = new MapWrapperSeq(map).asJava
    map
  }

  override def clone() = new ClassNodeWrapper(classNode)
}

private[asm] trait NodeImplicits {
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
  implicit class RichMethodNode(mn: MethodNode) {
    def methodName = MethodName(mn.name, mn.desc)
  }
  implicit class RichFieldNode(fn: FieldNode) {
    def fieldName = FieldName(fn.name, fn.desc)
  }
  implicit def classNodeWrapper2ClassNode(wrapper: ClassNodeWrapper) = wrapper.classNode
}