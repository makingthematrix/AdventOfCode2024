val _scalaVersion = "3.6.2"

organization := "io.github.makingthematrix"
name := "AdventOfCode2024"
licenses := Seq("MIT" -> url("https://www.gnu.org/licenses/mit.en.html"))
ThisBuild / scalaVersion := _scalaVersion
ThisBuild / versionScheme := Some("semver-spec")
Test / scalaVersion := _scalaVersion

enablePlugins(JmhPlugin)

val standardOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding",
  "utf8"
)

val scala3Options = Seq(
  "-explain",
  "-Wsafe-init",
  "-Ycheck-all-patmat",
  "-Wunused:imports",
  "-experimental"
)

// "-opt:inline,simplify-jumps,redundant-casts,box-unbox,nullness-tracking,closure-invocations,allow-skip-core-module-init,allow-skip-class-loading"

developers := List(
  Developer(
    "makingthematrix",
    "Maciej Gorywoda",
    "makingthematrix@protonmail.com",
    url("https://github.com/makingthematrix"))
)

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2024",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    ),
    scalacOptions ++= standardOptions ++ scala3Options
  )

