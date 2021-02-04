ThisBuild / scalaVersion := "3.0.0-M3"
ThisBuild / organization := "org.elevate-lang"

lazy val commonSettings = Seq(
  javaOptions ++= Seq("-Xss16m"),

  scalacOptions ++= {
    if (isDotty.value) Seq(
      "-source:3.0-migration",
      "-Xfatal-warnings",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:reflectiveCalls"
    ) else Seq(
      "-Wunused:nowarn",
      "-Xfatal-warnings",
      "-Xlint:-unused",
      "-Ymacro-annotations",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:reflectiveCalls"
    )
  },

  fork := true
)

lazy val elevate = (project in file("."))
  .settings(
    name    := "elevate",
    version := "1.0",

    commonSettings,

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % "test"
  )

