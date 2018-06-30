import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4"
    )),
    name := "kata--prison-transport",
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  )
