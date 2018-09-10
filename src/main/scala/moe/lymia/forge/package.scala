package moe.lymia

import java.io.File
import java.net.URL

import scala.collection.mutable
import scala.sys.process._
import scala.language.postfixOps

package object forge {
  def cacheFunction[A, B](fn: A => B) = {
    val cache = new mutable.HashMap[A, B]
    a: A => cache.getOrElse(a, {
      val v = fn(a)
      cache.put(a, v)
      v
    })
  }

  lazy val minecraftDirectory = {
    val os = System.getProperty("os.name").toLowerCase
    val userHome = System.getProperty("user.home", ".")
    if(os.contains("win")) {
      val appData = System.getenv("APPDATA")
      new File(if(appData != null) appData else userHome, ".minecraft/")
    }
    else if(os.contains("mac")) new File(userHome, "Library/Application Support/minecraft")
    else if(os.contains("linux") || os.contains("unix")) new File(userHome, ".minecraft/")
    else new File(userHome, "minecraft/")
  }

  private[forge] def download(url: URL, target: File) = {
    if(!target.getParentFile.exists)
      if(!target.getParentFile.mkdirs())
        sys.error(s"Failed to create parent directory of $target")
    url #> target !!
  }
}
