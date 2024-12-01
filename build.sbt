// based on http://caryrobbins.com/dev/sbt-publishing/

val _scalaVersion = "3.5.2"

organization := "io.github.makingthematrix"
sonatypeProfileName := "io.github.makingthematrix"
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
  "-Ysafe-init",
  "-Ycheck-all-patmat",
  "-Wunused:imports"
)

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
      //Test dependencies
      "org.scalameta" %% "munit" % "0.7.29" % "test"
    ),
    scalacOptions ++= standardOptions ++ scala3Options
  )

testFrameworks += new TestFramework("munit.Framework")
