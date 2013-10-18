name := "testMacros2"

version := "1.0"

scalaVersion := "2.11.0-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions += "-feature"

scalacOptions += "-deprecation"

libraryDependencies += "org.scala-lang"%"scala-compiler"%"2.11.0-SNAPSHOT"

libraryDependencies += "org.scala-lang" % "scala-reflect" %"2.11.0-SNAPSHOT"

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
