ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "kamin"
  )

libraryDependencies += "org.jline" % "jline" % "3.27.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
libraryDependencies += "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % Test

