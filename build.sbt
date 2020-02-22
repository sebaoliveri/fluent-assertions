name := "fluent-assertions"
organization := "assertions"

version := "0.9.32"

scalaVersion := "2.13.1"

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