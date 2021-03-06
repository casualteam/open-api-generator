import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  
  lazy val swaggerParser = "io.swagger.parser.v3" % "swagger-parser" % "2.0.16"
  lazy val betterFiles = "com.github.pathikrit" %% "better-files" % "3.8.0"
}
