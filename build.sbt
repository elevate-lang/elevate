lazy val elevate = (project in file("."))
  .settings(
    name    := "elevate",
    organization := "org.elevate-lang",
    version := "1.0",

    javaOptions ++= Seq("-Xss16m"),

    scalaVersion := "3.0.0-RC3",
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-indent",
      "-new-syntax",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:reflectiveCalls"
    ),

    fork := true,

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.8" % "test"
  )
