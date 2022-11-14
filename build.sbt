ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "2.5.0"
ThisBuild / organization     := "edu.berkeley.cs"

val chiselVersion = "3.5.5"

lazy val root = (project in file("."))
  .settings(
    name := "riscv-mini",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.5" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin(("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion).cross(CrossVersion.full))
  )
