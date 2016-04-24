val disciplineVersion = "0.4"
val scalaCheckVersion = "1.12.4"
val scalaTestVersion = "3.0.0-M7"
val spireVersion = "0.11.0"

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-laws" % spireVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.typelevel" %% "discipline" % disciplineVersion % "test",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

scalaVersion := "2.11.8"

scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ))

resolvers ++= Seq(
  "bintray/non" at "http://dl.bintray.com/non/maven",
  "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

homepage := Some(url("https://github.com/denisrosset/spire-cyclo"))

licenses += ("GPL-3.0", url("http://opensource.org/licenses/GPL-3.0"))

bintrayRepository := "maven"

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
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
