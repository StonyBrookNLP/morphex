import BuildSettings._

name := "morphex"

description := "A morphological feature extractor."

version := "0.1.0-SNAPSHOT"

scalacOptions in ThisBuild ++= Seq("-feature")

scalaVersion := "2.10.3"

unmanagedBase := baseDirectory.value / "lib"

lazy val root = Project(id = "morphex", base = file("."), settings = globalBuildSettings)

