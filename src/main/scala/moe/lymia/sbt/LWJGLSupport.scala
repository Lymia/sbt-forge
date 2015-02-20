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

package moe.lymia.sbt

import sbt._

import java.io.FileNotFoundException

import Keys._
import scala.util.Properties
import scala.language.postfixOps

object LWJGLSupport extends Plugin {
  object lwjgl {
    val version = SettingKey[String]("lwjgl-version")

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

  // Define Tasks
  private def lwjglCopyTask: Def.Initialize[Task[Seq[File]]] =
    (streams, lwjgl.copyDir, lwjgl.org, lwjgl.nativesJarName, lwjgl.os, ivyPaths) map { 
      (s, dir, org, jarName, dos, ivys) =>
      val (tos, ext) = dos
      val endness = Properties
        .propOrNone("os.arch")
        .filter(_.contains("64"))
        .map(_ => "64")
        .getOrElse("")

      s.log.info("Copying files for %s%s" format(tos, endness))

      val target = dir / tos
      s.log.debug("Target directory: %s" format target)

      if (target.exists) {
        s.log.info("Skipping because of existence: %s" format(target))
        Nil
      } else {
        val nativeLocation = pullNativeJar(org, jarName, ivys.ivyHome)

        if (nativeLocation.exists) {
          s.log.debug("Natives found at %s" format nativeLocation)
          val filter = new SimpleFilter(_.endsWith(ext))
          s.log.debug("Unzipping files ending with %s" format ext)

          IO.unzip(nativeLocation, target.asFile, filter)

          // House keeping - to be used in old method
          (target / tos * "*").get foreach { f =>
            IO.copyFile(f, target / f.name)
          }

          // Return the managed LWJGL resources
          target * "*" get
        } else {
          s.log.warn("""|You do not have the LWJGL natives installed %s.
                        |Consider requiring LWJGL through LWJGLPlugin.lwjglSettings and running
                        |again.""".stripMargin.format(nativeLocation))
          Nil
        }
      }
    }

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

    jars.get.filter(correct).headOption.getOrElse {
      throw new java.io.FileNotFoundException(
        "No Natives found in: %s" format(jarBase)
      )
    }
  }

  lazy val lwjglSettings: Seq[Setting[_]] = Seq (
    lwjgl.org := "org.lwjgl.lwjgl",

    libraryDependencies <++=
      (lwjgl.version, lwjgl.org, lwjgl.os) { 
        (v, org, os) => Seq(
          org % "lwjgl" % v, 
          org % "lwjgl_util" % v,
          org % "lwjgl-platform" % v classifier "natives-" + (os._1)
        )
      },

    lwjgl.version := "2.9.1",

    lwjgl.nativesJarName <<= (lwjgl.version, lwjgl.os) {
      "lwjgl-platform-" + _ + "-natives-" + _._1
    },

    lwjgl.os := defineOs,
    lwjgl.copyDir <<= (target) (_ / "lwjgl-natives"),

    lwjgl.copyNatives <<= lwjglCopyTask,
    (run in Runtime) <<= (run in Runtime) dependsOn lwjgl.copyNatives,

    cleanFiles <+= lwjgl.copyDir,

    fork := true,
    javaOptions <+= (lwjgl.copyDir, lwjgl.os).map{ (dir, os) => 
      "-Dorg.lwjgl.librarypath=%s".format(dir / os._1)
    }
  )
}
