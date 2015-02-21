package moe.lymia.sbt

import sbt._

import scala.collection.mutable.HashMap

package object forge {
  def cacheFunction[A, B](fn: A => B) = {
    val cache = new HashMap[A, B]
    (a: A) => cache.get(a).getOrElse {
      val v = fn(a)
      cache.put(a, v)
      v
    }
  }

  lazy val minecraftDirectory = {
    val os = System.getProperty("os.name").toLowerCase
    val userHome = System.getProperty("user.home", ".")
    if(os.contains("win")) {
      val appData = System.getenv("APPDATA")
      new File(if(appData != null) appData else userHome, ".minecraft/");
    }
    else if(os.contains("mac")) new File(userHome, "Library/Application Support/minecraft")
    else if(os.contains("linux") || os.contains("unix")) new File(userHome, ".minecraft/")
    else new File(userHome, "minecraft/")
  }
}
