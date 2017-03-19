import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.morambro",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "GameController",
    libraryDependencies += scalaTest % Test, 
    libraryDependencies += "org.scala-lang" % "scala-actors" % "2.11.8",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"
  )
