organization := "org.gnieh"

name := "docdiff"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.gnieh" %% "tekstlib" % "0.1.0-SNAPSHOT"
