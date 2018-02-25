import com.typesafe.sbt.SbtStartScript

name := "subtitle-fixer"

scalaVersion := "2.12.4"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

seq(SbtStartScript.startScriptForJarSettings: _*)
