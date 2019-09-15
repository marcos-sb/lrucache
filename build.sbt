name := "lrucache"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.23"
libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % Test
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.25" % Test