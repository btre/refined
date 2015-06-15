enablePlugins(GitVersioning)

lazy val root = project.in(file("."))
  .aggregate(refinedJVM, refinedJS)
  .settings(noPublishSettings)

val gitRepo = "git@github.com:fthomas/refined.git"

/*
val tutSource = "docs/src"
val tutTarget = "docs"

tutSettings
tutScalacOptions := scalacOptions.value
tutSourceDirectory := file(tutSource)

lazy val tutUpdate = taskKey[Unit]("")
tutUpdate := tut.value.foreach { case (compiled, name) =>
  IO.copyFile(compiled, file(s"$tutTarget/$name"))
}
*/


lazy val refined = crossProject.in(file("."))
  .settings(
    name := "refined",
    description := "Refinement types for Scala",

    organization := "eu.timepit",
    homepage := Some(url("https://github.com/fthomas/refined")),
    startYear := Some(2015),
    licenses += "MIT" -> url("http://opensource.org/licenses/MIT"),

    scmInfo := Some(ScmInfo(homepage.value.get,
      "scm:git:https://github.com/fthomas/refined.git",
      Some(s"scm:git:$gitRepo"))),

    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xfuture",
      "-Xlint",
      //"-Xlog-implicits",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard"
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.chuusai" %%% "shapeless" % "2.2.2",
      "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"
    ),

    initialCommands := """
      import eu.timepit.refined._
      import eu.timepit.refined.boolean._
      import eu.timepit.refined.char._
      import eu.timepit.refined.collection._
      import eu.timepit.refined.generic._
      import eu.timepit.refined.implicits._
      import eu.timepit.refined.numeric._
      import eu.timepit.refined.string._
      import shapeless.{ ::, HList, HNil }
      import shapeless.nat._
      import shapeless.tag.@@
    """,

    git.useGitDescribe := true,

    wartremoverErrors in (Compile, compile) ++= Warts.unsafe diff
      Seq(Wart.Any, Wart.AsInstanceOf, Wart.NonUnitStatements, Wart.Throw),

    // doc settings

    scalacOptions in (Compile, doc) ++= Seq(
      "-diagrams",
      "-diagrams-debug",
      "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
      "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
    ),

    autoAPIMappings := true,
    apiURL := Some(url("http://fthomas.github.io/refined/latest/api/"))
  )
  .settings(publishSettings: _*)
  .settings(styleSettings: _*)
  .jvmSettings()
  .jsSettings()

lazy val refinedJVM = refined.jvm
lazy val refinedJS = refined.js


/*

// site settings

site.settings
site.includeScaladoc()
ghpages.settings
git.remoteRepo := gitRepo

// release settings

import ReleaseTransformations._
releasePublishArtifactsAction := PgpKeys.publishSigned.value
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  releaseStepTask(GhPagesKeys.pushSite),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

*/

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <developers>
      <developer>
        <id>fthomas</id>
        <name>Frank S. Thomas</name>
        <url>https://github.com/fthomas</url>
      </developer>
    </developers>
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val styleSettings =
  scalariformSettings

addCommandAlias("validate", ";clean;coverage;test;scalastyle;test:scalastyle;doc;tutUpdate")
