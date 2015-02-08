sbtPlugin := true

name := "sbt-forge"

organization := "moe.lymia"

version := "0.1.0"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

scalaVersion := "2.10.4"

libraryDependencies += "org.ow2.asm" % "asm-debug-all" % "5.0.3"

libraryDependencies += "com.github.jponge" % "lzma-java" % "1.2"

libraryDependencies += "com.nothome" % "javaxdelta" % "2.0.1"

pomExtra := (
  <url>https://github.com/Lymia/sbt-forge</url>
  <licenses>
    <license>
      <name>The MIT License</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:Lymia/sbt-forge.git</url>
    <connection>scm:git:git@github.com:Lymia/sbt-forge.git</connection>
  </scm>
  <developers>
    <developer>
      <id>Lymia</id>
      <name>Lymia Aluysia</name>
      <url>http://github.com/Lymia</url>
    </developer>
  </developers>
)
