name := "GameOfLife"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

version := "0.1"

scalaVersion := "2.12.4"
enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
