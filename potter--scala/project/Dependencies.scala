import sbt._

object Dependencies {
  lazy val akkaHttpVersion = "10.0.10"
  lazy val akkaVersion    = "2.5.4"

  lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
  lazy val akkaHttpSprayJson = "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
  lazy val akkaHttpXml = "com.typesafe.akka" %% "akka-http-xml" % akkaHttpVersion
  lazy val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  lazy val akkaHttpTestkit = "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
}
