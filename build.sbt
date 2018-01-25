lazy val shared = crossProject.crossType(CrossType.Pure).in(file("shared")).settings(
  libraryDependencies += "io.suzaku" %% "boopickle" % "1.2.6",
  name := "model",
  organization := "com.ffbecalc",
  version := "0.1-SNAPSHOT"
)
lazy val sharedJS = shared.js
lazy val sharedJVM = shared.jvm
lazy val jvm = project.in(file("jvm")).settings(
  libraryDependencies ++=
    "io.circe" %% "circe-core" % "0.8.0" ::
    "io.circe" %% "circe-parser" % "0.8.0" ::
    "io.circe" %% "circe-optics" % "0.8.0" ::
    "io.suzaku" %% "boopickle" % "1.2.6" ::
    Nil
).dependsOn(sharedJS)

lazy val root = project.in(file(".")).dependsOn(sharedJS)

val versionCode = taskKey[String]("generate version code")
val versionFile = taskKey[File]("generate version file from version code")
val verifyVersion = taskKey[Unit]("verify versionFile+buildInfo")
val timestamp = taskKey[Long]("generate current time")

timestamp := System.currentTimeMillis

versionCode := {
  val sdf = new java.text.SimpleDateFormat("yyyyMMddHHmm")
  sdf.format(timestamp.value)
}

enablePlugins(ScalaJSPlugin)
enablePlugins(WorkbenchPlugin)
enablePlugins(ScalaJSBundlerPlugin)
enablePlugins(BuildInfoPlugin)

name := "ffbecalc"

buildInfoKeys := Seq[BuildInfoKey](versionCode, timestamp)
buildInfoPackage := "com.ffbecalc"

scalaVersion in Global := "2.12.3"

libraryDependencies += "io.github.outwatch" %%% "outwatch" % "0.10.2"
libraryDependencies += "io.suzaku" %%% "boopickle" % "1.2.6"

versionFile := {
  val f = baseDirectory.value / "versionCode"
  IO.writeLines(f, versionCode.value :: Nil)
  f
}

verifyVersion := {
  val vc = IO.readLines(versionFile.value).head.trim
  val hasVC = IO.readLines((buildInfo in Compile).value.head).exists(
    _.contains(vc))
  if (!hasVC) throw new MessageOnlyException(s"buildInfo does not contain $vc")
}

versionFile := (versionFile triggeredBy (buildInfo in Compile)).value
verifyVersion := (verifyVersion triggeredBy (buildInfo in Compile)).value

refreshBrowsers :=
  (refreshBrowsers triggeredBy (webpack in fastOptJS in Compile)).value

webpackBundlingMode := BundlingMode.LibraryOnly()

scalaJSUseMainModuleInitializer := true
