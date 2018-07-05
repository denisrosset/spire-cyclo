val scalaVersions = Map("2.11" -> "2.11.12", "2.12" -> "2.12.6")

val disciplineVersion = "0.8"
val scalaCheckVersion = "1.13.5"
val scalaTestVersion = "3.0.5"
val spireVersion = "0.16.0"

lazy val spireCyclo = (project in file("."))
  .settings(moduleName := "cyclo")
  .settings(cycloSettings)
  .settings(noPublishSettings)
  .aggregate(core, laws, tests)
  .dependsOn(core, laws, tests)

lazy val core = (project in file("core"))
  .settings(moduleName := "cyclo-core")
  .settings(cycloSettings)

lazy val laws = (project in file("laws"))
  .settings(moduleName := "cyclo-laws")
  .settings(cycloSettings: _*)
  .settings(testSettings:_*)
  .dependsOn(core)

lazy val tests = (project in file("tests"))
  .settings(moduleName := "cyclo-tests")
  .settings(cycloSettings: _*)
  .settings(testSettings:_*)
  .settings(noPublishSettings:_*)
  .dependsOn(core, laws)

lazy val cycloSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val buildSettings = Seq(
  name := "cyclo",
  organization := "net.alasc",
  scalaVersion := scalaVersions("2.12"),
  crossScalaVersions := scalaVersions.values.toSeq
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-language:existentials",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )),
  resolvers ++= Seq(
    "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies += "org.typelevel" %% "spire" % spireVersion,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
) ++ warnUnusedImport ++ selectiveOptimize

lazy val publishSettings = Seq(
  scmInfo := Some(ScmInfo(url("https://github.com/denisrosset/cyclo"), "scm:git:git@github.com:denisrosset/cyclo.git")),
  homepage := Some(url("http://github.com/denisrosset/cyclo")),
  licenses += ("MIT", url("http://opensource.org/licenses/GPL-2.0")),
  bintrayRepository := "maven",
  publishArtifact in Test := false,
  releaseCrossBuild := true
)

// do not optimize on Scala 2.10 because of optimizer bug, see SI-3882
lazy val selectiveOptimize = 
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => Seq()
      case Some((2, 11)) => Seq("-optimize")
      case Some((2, 12)) => Seq()
      case _ => sys.error("Unknown Scala version")
    }
  }

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "spire-laws" % spireVersion % "test",
    "org.scalatest" %% "scalatest" % scalaTestVersion,
    "org.typelevel" %% "discipline" % disciplineVersion,
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  )
)

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

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)
