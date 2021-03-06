
lazy val scala212 = "2.12.9"
lazy val scala213 = "2.13.1"
lazy val supportedScalaVersions = List(scala212, scala213)
lazy val akkaVersion = "2.6.1"

name := "fluent-assertions"
organization := "nulluncertainty"
version := "2.0.2"
scalaVersion := scala213
crossScalaVersions := supportedScalaVersions

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/sebaoliveri/fluent-assertions"))

pomExtra :=
  <scm>
    <connection>
      scm:git:git://github.com/sebaoliveri/fluent-assertions.git
    </connection>
    <url>
      https://github.com/sebaoliveri/fluent-assertions
    </url>
  </scm>
    <developers>
      <developer>
        <id>sebasoliveri</id>
        <name>Sebastian Oliveri</name>
        <email>sebasoliveri@gmail.com</email>
      </developer>
    </developers>

publishTo := Some(
  "chalten" at
    "https://chalten.jfrog.io/artifactory/releases")
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
publishMavenStyle := true