/*
Code based off sbt-lwjgl-plugin, available under the following license:

Copyright (c) 2011 Philip Cali 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

package moe.lymia.forge

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

import scala.language.postfixOps
import scala.util.Properties

object LWJGLPlugin extends AutoPlugin {
  object autoImport {
    object lwjgl {
      val version = SettingKey[String]("lwjgl-version")

      val libraries = SettingKey[Seq[ModuleID]]("lwjgl-libraries",
        "A list of lwjgl libraries.")

      val copyDir = SettingKey[File]("lwjgl-copy-directory",
        "This is where lwjgl resources will be copied")

      val os = SettingKey[(String, String)]("lwjgl-os",
        "This is the targeted OS for the build. Defaults to the running OS.")

      val org = SettingKey[String]("lwjgl-org",
        "Custom lwjgl maven organization.")

      val nativesJarName = SettingKey[String]("lwjgl-natives-jar-name",
        "This name will be used to pull the specific jar for loading the runtime.")

      val copyNatives = TaskKey[Seq[File]]("lwjgl-copy-natives",
        "Copies the lwjgl library from natives jar to target")
    }
  }
  import autoImport.lwjgl

  // Helper methods 
  def defineOs = System.getProperty("os.name").toLowerCase.take(3).toString match {
    case "lin" => ("linux", "so")
    case "mac" | "dar" => ("osx", "lib")
    case "win" => ("windows", "dll")
    case "sun" => ("solaris", "so")
    case _ => ("unknown", "")
  }

  private def pullNativeJar(org: String, jarName: String, ivyHome: Option[File]) = { 
    val correct = (f: File) =>
      f.getName == "%s.jar".format(jarName)

    val base = ivyHome.getOrElse(Path.userHome / ".ivy2")

    val jarBase = base / "cache" / org / "lwjgl-platform" / "jars"
    val jars = jarBase * "*.jar"

    jars.get.find(correct).getOrElse {
      throw new java.io.FileNotFoundException(
        s"No Natives found in: $jarBase"
      )
    }
  }

  override val requires = JvmPlugin
  override lazy val projectSettings = Seq (
    lwjgl.org := "org.lwjgl.lwjgl",

    lwjgl.libraries := Seq(
      lwjgl.org.value % "lwjgl" % lwjgl.version.value,
      lwjgl.org.value % "lwjgl_util" % lwjgl.version.value,
      lwjgl.org.value % "lwjgl-platform" % lwjgl.version.value classifier s"natives-${lwjgl.os.value._1}",
    ),
    allDependencies ++= lwjgl.libraries.value,

    lwjgl.version := "2.9.1",

    lwjgl.nativesJarName := s"lwjgl-platform-${lwjgl.version.value}-natives-${lwjgl.os.value._1}",

    lwjgl.os := defineOs,
    lwjgl.copyDir := target.value / "lwjgl-natives",

    lwjgl.copyNatives := {
      val log = streams.value.log
      val (osName, nativeLibExtension) = lwjgl.os.value

      val bits =
        Properties
        .propOrNone("os.arch")
        .filter(_.contains("64"))
        .map(_ => "64")
        .getOrElse("")

      log.info(s"Copying files for $osName$bits")

      val target = lwjgl.copyDir.value / osName
      log.debug(s"Target directory: $target")

      if (target.exists) {
        log.info(s"Skipping because of existence: $target")
        Nil
      } else {
        val nativeLocation = pullNativeJar(lwjgl.org.value, lwjgl.nativesJarName.value, ivyPaths.value.ivyHome)

        if (nativeLocation.exists) {
          log.debug("Natives found at %s" format nativeLocation)
          val filter = new SimpleFilter(_.endsWith(nativeLibExtension))
          log.debug("Unzipping files ending with %s" format nativeLibExtension)

          IO.unzip(nativeLocation, target.asFile, filter)

          // House keeping - to be used in old method
          (target / osName * "*").get foreach { f =>
            IO.copyFile(f, target / f.name)
          }

          // Return the managed LWJGL resources
          target * "*" get
        } else {
          log.warn(
            s"""You do not have the LWJGL natives installed $nativeLocation.
               |Consider requiring LWJGL through LWJGLPlugin.lwjglSettings and running
               |again.""".stripMargin)
          Nil
        }
      }
    },
    (run in Runtime) := ((run in Runtime) dependsOn lwjgl.copyNatives).evaluated,

    cleanFiles += lwjgl.copyDir.value,

    fork := true,
    javaOptions += s"-Dorg.lwjgl.librarypath=${lwjgl.copyDir.value / lwjgl.os.value._1}"
  )
}
