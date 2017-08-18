enablePlugins(ScalaJSPlugin)
enablePlugins(WorkbenchPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "yaffbedb"

scalaVersion := "2.12.3"

//scalaJSUseMainModuleInitializer := true

libraryDependencies += "io.circe" %%% "circe-core" % "0.8.0"
libraryDependencies += "io.circe" %%% "circe-parser" % "0.8.0"
libraryDependencies += "io.github.outwatch" %%% "outwatch" % "0.10.1"

refreshBrowsers <<= refreshBrowsers triggeredBy (webpack in fastOptJS in Compile)

lazy val shared = project.in("shared").settings(
)
lazy val jvm = project.in("jvm").settings(
libraryDependencies += "io.circe" %%% "circe-core" % "0.8.0",
libraryDependencies += "io.circe" %%% "circe-parser" % "0.8.0"
)
