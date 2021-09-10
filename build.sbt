scalaVersion := "2.13.6"

name := "we-wanna-be-database-masters"
organization := "com.chatwork"
version := "1.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.14.1")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
