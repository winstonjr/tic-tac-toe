import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "cc.atleastonce"
ThisBuild / organizationName := "atleastonce"

lazy val root = (project in file("."))
  .settings(
    name := "tic-tac-toe",
    libraryDependencies += scalaTest % Test
  )

mainClass in (Compile, run) := Some("boardgame.Game")
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
