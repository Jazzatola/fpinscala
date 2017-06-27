name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.2"

crossPaths := false

scalacOptions ++= Seq(
  "-feature"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"
)