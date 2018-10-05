package moe.lymia.forge

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object LWJGLNativesPlugin extends AutoPlugin {
  object autoImport {
    val lwjglVersion = settingKey[String]("The version of LWJGL to use.")
    val lwjglNativeDeps = settingKey[Seq[ModuleID]]("The LWJGL native dependencies.")
    val lwjglNativesDir = settingKey[File]("This is where lwjgl natives will be copied")
    val lwjglNatives = taskKey[File]("Copy lwjgl's natives into lwjglNativesDir")
  }
  import autoImport._

  private def findModules(dep: ModuleID, classpath: Classpath) =
    classpath.filter(x => x.get(moduleID.key).fold(false)(x => Utils.cleanModuleID(x) == dep))

  override val requires = JvmPlugin
  override lazy val projectSettings = Seq (
    lwjglVersion := "2.9.1",
    lwjglNativeDeps := Seq(
      "org.lwjgl.lwjgl" % "lwjgl-platform" % lwjglVersion.value classifier "natives-windows",
      "org.lwjgl.lwjgl" % "lwjgl-platform" % lwjglVersion.value classifier "natives-osx",
      "org.lwjgl.lwjgl" % "lwjgl-platform" % lwjglVersion.value classifier "natives-linux"
    ),
    lwjglNativesDir := target.value / "lwjgl-natives",

    lwjglNatives := {
      val log = streams.value.log
      val target = lwjglNativesDir.value
      val classpath = (managedClasspath in Compile).value // TODO: What scope is best here?
      for (dep <- findModules("org.lwjgl.lwjgl" % "lwjgl-platform" % lwjglVersion.value, classpath)) {
        log.info(s"Extracting LWJGL natives from ${dep.data} to $target...")
        val filter = new SimpleFilter(!_.startsWith("META-INF/"))
        IO.unzip(dep.data, target.asFile, filter)
      }
      target
    },

    fork := true,
    javaOptions += s"-Dorg.lwjgl.librarypath=${lwjglNatives.value}"
  )
}
