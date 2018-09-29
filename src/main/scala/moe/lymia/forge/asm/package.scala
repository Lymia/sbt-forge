package moe.lymia.forge

import java.io.InputStream

import org.objectweb.asm.{ClassReader, ClassWriter}
import org.objectweb.asm.tree.ClassNode

package object asm extends NodeImplicits with JarImplicits {
  def dumpClassNode(cn: ClassNode) = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cn.accept(cw)
    cw.toByteArray
  }
  def readClassNode(in: InputStream) = {
    val cr = new ClassReader(in)
    val cn = new ClassNode()
    cr.accept(cn, ClassReader.EXPAND_FRAMES)
    cn
  }
}