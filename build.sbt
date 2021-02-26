ThisBuild / scalaVersion := "3.0.0-RC1"
ThisBuild / organization := "org.elevate-lang"

lazy val commonSettings = Seq(
  javaOptions ++= Seq("-Xss16m"),

  scalacOptions ++= {
    if (isDotty.value) Seq(
//      "-Xfatal-warnings",
      "-indent",
      "-new-syntax",
//      "-rewrite",
      "-deprecation",
      "-feature",
      "-unchecked",
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
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
  )

