lazy val root = (project in file(".")).
  settings(
    name := "mommifier",
    inThisBuild(List(
      organization := "io.ryuichi",
      scalaVersion := "2.12.6"
    )),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
