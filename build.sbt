ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "org.elevate-lang"

lazy val commonSettings = Seq(
  javaOptions ++= Seq("-Xss16m"),

  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Xlint:-unused",
    "-Xmax-classfile-name", "100",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls"
  ),

  fork := true,

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
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
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0",

    // JUnit
    libraryDependencies += "junit" % "junit" % "4.11",

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test",

    // Silencer: Scala compiler plugin for warning suppression
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.3" cross CrossVersion.full),
      "com.github.ghik" %% "silencer-lib" % "1.4.3" % Provided cross CrossVersion.full
    )
  )

lazy val elevateMacros = (project in file("macros"))
  .settings(
    name := "macros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

