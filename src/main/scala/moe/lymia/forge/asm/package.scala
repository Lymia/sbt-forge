package moe.lymia.forge

import org.objectweb.asm.{ClassReader, ClassWriter}
import org.objectweb.asm.tree.ClassNode

package object asm extends NodeImplicits with JarImplicits {
  def dumpClassNode(cn: ClassNode) = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cn.accept(cw)
    cw.toByteArray
  }
  def readClassNode(data: Array[Byte], flags: Int = 0) = {
    val cr = new ClassReader(data)
    val cn = new ClassNode()
    cr.accept(cn, flags)
    cn
  }

  def readClassSymbols(data: Array[Byte]) =
    readClassNode(data, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
}