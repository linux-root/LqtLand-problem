ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(name := "LtqLand-problem",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.4.2",
      "org.scalatest" %% "scalatest" % "3.2.14" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test
    )
  )