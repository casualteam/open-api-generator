import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "casualteam.openapigenerator"
ThisBuild / organizationName := "casualteam"

lazy val root = (project in file("."))
  .enablePlugins(SbtTwirl)
  .settings(
    name := "open-api-generator",
    libraryDependencies ++= Seq(
      swaggerParser,
      scalaTest % Test
    ),
    unmanagedSourceDirectories in Compile += (baseDirectory( _ / "src"/ "main" / "twirl" )).value,
    sourceDirectories in(Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value
  )