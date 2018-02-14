import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.ryuichi",
      scalaVersion := "2.12.4"
    )),
    name := "kata--coloured-triangles",
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  )
