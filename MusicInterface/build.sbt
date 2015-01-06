name := "IRGen"

organization := "epfl"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-feature", "-language:postfixOps", "-language:existentials", "-language:implicitConversions")

fork := true

libraryDependencies += "com.googlecode.soundlibs" % "tritonus-share" % "0.3.7-1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
