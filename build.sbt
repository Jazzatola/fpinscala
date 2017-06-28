name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.2"

crossPaths := false

scalacOptions ++= Seq(
  "-feature"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)