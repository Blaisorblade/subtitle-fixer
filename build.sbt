import com.typesafe.sbt.SbtStartScript

name := "subtitle-fixer"

scalaVersion := "2.9.2"

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

version := "0.1"

seq(SbtStartScript.startScriptForJarSettings: _*)
