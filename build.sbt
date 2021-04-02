ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "1.0"
ThisBuild / organization     := "be.slechten"
ThisBuild / organizationName := "slechten"

lazy val root = (project in file("."))
  .settings(
    name := "ZioAnomalyCrawler",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.5",
      "dev.zio" %% "zio-test" % "1.0.5" % Test,
      "dev.zio" %% "zio-test-sbt" % "1.0.5" % Test,
      "com.lihaoyi" %% "cask" % "0.7.7",
      "org.scalactic" %% "scalactic" % "3.2.5",
      "org.scalatest" %% "scalatest" % "3.2.5" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
