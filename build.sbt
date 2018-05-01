val scala211Version = "2.11.12"
val scala212Version = "2.12.6"

val disciplineVersion = "0.8"
val scalaCheckVersion = "1.13.5"
val scalaTestVersion = "3.0.5"
val spireVersion = "0.15.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % spireVersion,
  "org.typelevel" %% "spire-laws" % spireVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.typelevel" %% "discipline" % disciplineVersion % "test",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

scalaVersion := scala212Version

crossScalaVersions := Seq(scala211Version, scala212Version)

scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ))

resolvers ++= Seq(
  "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

homepage := Some(url("https://github.com/denisrosset/spire-cyclo"))

licenses += ("GPL-3.0", url("http://opensource.org/licenses/GPL-3.0"))

organization := "net.alasc"

bintrayRepository := "maven"

releaseCrossBuild := true

publishArtifact in Test := false

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
