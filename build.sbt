version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.12"

name := "AdventOfCode2023"

idePackagePrefix.withRank(KeyRanks.Invisible) := Some("me.astynax")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.3.0"
libraryDependencies +=  "com.lihaoyi" %% "fastparse" % "3.0.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

scalacOptions ++= Seq(
  "-Werror",
  "-deprecation",
  "-feature"
)
