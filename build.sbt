organization := "scalastudygroup"

name := "dash"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"

scalacOptions += "-language:implicitConversions"
