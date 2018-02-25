import com.typesafe.sbt.SbtStartScript

name := "subtitle-fixer"

scalaVersion := "2.10.7"

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

seq(SbtStartScript.startScriptForJarSettings: _*)
