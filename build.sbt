import sbt.richFile

name                       := "record-calculus"
description                := "An implementation of the Record Calculus in Scala"
organization               := "de.tu_dortmund.cs.ls14"
version                    := "1.0.0-SNAPSHOT"

scalaVersion               := "2.11.6"
scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"
resolvers += Resolver.mavenLocal
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0-SNAPSHOT"

dependencyOverrides += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0-SNAPSHOT"

import org.scalastyle.sbt.ScalastylePlugin
scalastyleConfig <<= baseDirectory / "lib/scalastyle_config.xml"

val meta = """META.INF(.)*""".r
assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case n if n.startsWith("reference.conf") => MergeStrategy.concat
  case n if n.endsWith(".conf") => MergeStrategy.concat
  case meta(_) => MergeStrategy.discard
  case x => MergeStrategy.first
}

