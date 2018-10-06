package moe.lymia.forge.build

import java.io._
import java.util.jar._
import java.util.zip._

import com.nothome.delta.GDiffPatcher
import lzma.sdk.lzma.Decoder
import lzma.streams.LzmaInputStream
import sbt._

import scala.collection.JavaConverters._

sealed trait TargetStatus
object TargetStatus {
  case object Missing extends TargetStatus
  case class Exists(checksum: Int) extends TargetStatus
}

final case class PatchData(displayName: String, targetClassName: String, transformedClassName: String,
                           target: TargetStatus, data: Array[Byte], modifiedTime: Long) {
  override def toString = s"$displayName ($targetClassName -> $transformedClassName)"
}
object PatchData {
  def load(rin: InputStream, modifiedTime: Long = System.currentTimeMillis()) = {
    def in = new DataInputStream(rin)
    PatchData(in.readUTF(), in.readUTF(), in.readUTF(),
              if(in.readBoolean()) TargetStatus.Exists(in.readInt()) else TargetStatus.Missing, {
                val buffer = new Array[Byte](in.readInt())
                in.readFully(buffer)
                buffer
              }, modifiedTime)
  }
}

final case class PatchSet(patches: Map[String, PatchData]) {
  private def adlerHash(input: Array[Byte]) = {
    val hasher = new Adler32()
    hasher.update(input)
    hasher.getValue.toInt
  }
  def patchJar(sourceFile: File, targetFile: File, serverDepPrefixes: Seq[String], log: Logger) {
    log.info(s"Patching $sourceFile to $targetFile")
    val jarIn  = new ZipFile(sourceFile)
    val jarOut = new ZipOutputStream(new FileOutputStream(targetFile))
    val patcher = new GDiffPatcher

    def putEntry(name: String, modifiedTime: Long) = {
      val newEntry = new ZipEntry(name)
      newEntry.setTime(modifiedTime)
      jarOut.putNextEntry(newEntry)
    }

    for (entry <- jarIn.entries().asScala)
      if (!patches.contains(entry.getName) && !serverDepPrefixes.exists(x => entry.getName.startsWith(x))) {
        putEntry(entry.getName, entry.getTime)
        jarOut.write(IO.readBytes(jarIn.getInputStream(entry)))
      }
    for ((name, patch) <- patches) {
      log.debug(s"Applying $patch")
      putEntry(name, patch.modifiedTime)

      val entry = jarIn.getEntry(name)
      val origData = patch.target match {
        case TargetStatus.Missing =>
          if (entry != null) {
            log.error(s"$entry exist, but patch for $patch declares it as non-existant.")
            sys.error(s"File unexpectedly existant while processing $patch")
          }
          Array[Byte]()
        case TargetStatus.Exists(expected) =>
          if (entry == null) {
            log.error(s"$entry does not exist, but is required by patch for $patch.")
            sys.error(s"Patch target not found in $patch")
          }

          val data = IO.readBytes(jarIn.getInputStream(entry))
          val hash = adlerHash(data)
          if (hash != expected) {
            log.error(s"${entry.getName} does not match the checksum expected by patch for $patch. "+
                      "(expected: 0x"+expected.toHexString+", target: 0x"+hash.toHexString+")\n")
            sys.error(s"Checksum mismatch in $patch")
          }
          data
      }
      jarOut.write(patcher.patch(origData, patch.data))
    }

    jarIn.close()
    jarOut.close()
  }
}
object PatchSet {
  def load(in: JarInputStream, section: String): PatchSet = {
    val patchSet = new collection.mutable.HashMap[String, PatchData]
    var entry: JarEntry = null
    while ({entry = in.getNextJarEntry; entry != null}) {
      if (entry.getName.matches(s"binpatch/$section/.*\\.binpatch")) {
        val patch = PatchData.load(in, entry.getTime)
        patchSet.put(patch.targetClassName.replace(".", "/")+".class", patch)
      }
    }
    PatchSet(patchSet.toMap)
  }
  def load(in: File, section: String): PatchSet = {
    // TODO: Properly close streams
    val lzmaIn  = new LzmaInputStream(new FileInputStream(in), new Decoder())
    val byteOut = new ByteArrayOutputStream()
    val jarOut  = new JarOutputStream(byteOut)
    Pack200.newUnpacker().unpack(lzmaIn, jarOut)
    load(new JarInputStream(new ByteArrayInputStream(byteOut.toByteArray)), section)
  }
}