lazy val shared = crossProject.crossType(CrossType.Pure).in(file("shared"))
lazy val sharedJS = shared.js
lazy val jvm = project.in(file("jvm")).settings(
  libraryDependencies ++=
    "io.circe" %% "circe-core" % "0.8.0" ::
    "io.circe" %% "circe-parser" % "0.8.0" ::
    "io.suzaku" %% "boopickle" % "1.2.6" ::
    Nil
).dependsOn(sharedJS)

lazy val root = project.in(file(".")).dependsOn(sharedJS)
enablePlugins(ScalaJSPlugin)
enablePlugins(WorkbenchPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "yaffbedb"

scalaVersion in Global := "2.12.3"

//scalaJSUseMainModuleInitializer := true

libraryDependencies += "io.github.outwatch" %%% "outwatch" % "0.10.2"
libraryDependencies += "io.suzaku" %%% "boopickle" % "1.2.6"

refreshBrowsers in root :=
  (refreshBrowsers in root triggeredBy (webpack in fastOptJS in Compile in root)).value

