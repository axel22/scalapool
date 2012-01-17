
name := "scalapool"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"

scalacOptions ++= Seq("-Yinline")