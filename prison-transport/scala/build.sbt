lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.ryuichi",
      scalaVersion := "2.12.8"
    )),
    name := "prison-transport--scala",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )
