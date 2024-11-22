
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"  // Make sure this matches the Scala version you're using

lazy val root = (project in file("."))
  .settings(
    name := "CS476HW3",

    // Add the ScalaTest dependencies for testing
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.scalatest" %% "scalatest-featurespec" % "3.2.10" % Test
    )
  )

