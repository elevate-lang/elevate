ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "org.elevate-lang"

lazy val commonSettings = Seq(
  javaOptions ++= Seq("-Xss16m"),

  scalacOptions ++= Seq(
    "-Wunused:nowarn",
    "-Xfatal-warnings",
    "-Xlint:-unused",
    "-Ymacro-annotations",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls"
  ),

  fork := true
)

lazy val elevate = (project in file("."))
  .dependsOn(elevateMacros)
  .settings(
    name    := "elevate",
    version := "1.0",

    commonSettings,

    // Scala libraries
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,

    // JUnit
    libraryDependencies += "junit" % "junit" % "4.11",

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
  )

lazy val elevateMacros = (project in file("macros"))
  .settings(
    name := "elevateMacros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

