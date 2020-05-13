
lazy val scala212 = "2.12.9"
lazy val scala213 = "2.13.1"
lazy val supportedScalaVersions = List(scala212, scala213)
lazy val akkaVersion = "2.6.1"

name := "fluent-assertions"
organization := "nulluncertainty"
version := "1.1.0"
scalaVersion := scala213
crossScalaVersions := supportedScalaVersions

resolvers += Resolver.bintrayRepo("sebasoliveri", "maven")

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
  "bintray" at
    "https://api.bintray.com/maven/fluent-assertions/" +
      "releases/fluent-assertions/;publish=1")
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
publishMavenStyle := true