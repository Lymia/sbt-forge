package moe.lymia.sbt.forge

import sbt._
import scala.collection.JavaConversions._

import java.io._
import java.util.zip._
import java.util.jar._

import lzma.sdk.lzma.Decoder
import lzma.streams.LzmaInputStream

import com.nothome.delta.GDiffPatcher

object BinPatch {
  case class PatchData(name: String,
                       untransformedName: String, transformedName: String, 
                       inputChecksum: Option[Int], patchData: Array[Byte],
                       modifiedTime: Long) {
    lazy val patchName = name+" ("+untransformedName+" -> "+transformedName+")"
  }
  type PatchSet = Map[String, PatchData]

  def readPatchData(rin: InputStream, mt: Long) = {
    def in = new DataInputStream(rin)
    PatchData(in.readUTF(), in.readUTF(), in.readUTF(), 
              if(in.readBoolean()) Some(in.readInt()) else None,
              {
                val buffer = new Array[Byte](in.readInt())
                in.readFully(buffer)
                buffer
              }, mt)
  }
  def readPatchSetFromJar(in: JarInputStream, section: String) = {
    val patchSet = new collection.mutable.HashMap[String, PatchData]
    var entry: JarEntry = null
    while({entry = in.getNextJarEntry; entry != null}) {
      if(entry.getName.matches("binpatch/"+section+"/.*\\.binpatch")) {
        val patch = readPatchData(in, entry.getTime)
        patchSet.put(patch.untransformedName.replace(".", "/")+".class", patch)
      }
    }
    patchSet.toMap
  }
  def readPatchSet(in: File, section: String) = {
    val lzmaIn  = new LzmaInputStream(new FileInputStream(in), new Decoder())
    val byteOut = new ByteArrayOutputStream()
    val jarOut  = new JarOutputStream(byteOut)
    Pack200.newUnpacker().unpack(lzmaIn, jarOut)
    readPatchSetFromJar(new JarInputStream(new ByteArrayInputStream(byteOut.toByteArray)), section)
  }

  def adlerHash(input: Array[Byte]) = {
      val hasher = new Adler32()
      hasher.update(input)
      hasher.getValue.toInt
  }
  def patchJar(sourceFile: File, targetFile: File, patchSet: PatchSet, log: Logger) {
    log.info("Patching "+sourceFile+" to "+targetFile)
    val jarIn  = new ZipFile(sourceFile)
    val jarOut = new ZipOutputStream(new FileOutputStream(targetFile))
    val patcher = new GDiffPatcher
    for(entry <- jarIn.entries()) {
      patchSet.get(entry.getName) match {
        case Some(patch) =>
          log.debug("Applying patch "+patch.patchName)

          val newEntry = new ZipEntry(entry.getName)
          newEntry.setTime(patch.modifiedTime)
          jarOut.putNextEntry(newEntry)

          val data = IO.readBytes(jarIn.getInputStream(entry))
          patch.inputChecksum match {
            case None =>
              log.warn("Patch file "+patch.patchName+" declares no checksum for input file. "+
                       "This is not fully supported by ForgeGradle, and something is probably very wrong with your universal jar.")
            case Some(expected) =>
              val hash = adlerHash(data)
              if(hash != expected)
                sys.error(entry.getName+" does not match the checksum expected by "+patch.patchName+". "+
                          "The patch expects "+expected.toHexString+", while the checksum of the file is "+hash.toHexString+".")
          }
          jarOut.write(patcher.patch(data, patch.patchData))
        case None =>
          jarOut.putNextEntry(entry)
          IO.transfer(jarIn.getInputStream(entry), jarOut)
      }
    }
    jarIn.close()
    jarOut.close()
  }
}
