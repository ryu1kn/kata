import sbt._

object Dependencies {
  lazy val cucumberVersion = "2.0.1"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4"
  lazy val cucumber = "io.cucumber" %% "cucumber-scala" % cucumberVersion
  lazy val cucumberJunit = "io.cucumber" % "cucumber-junit" % cucumberVersion
}
