import sbt.richFile

name                       := "record-calculus"
description                := "An implementation of the Record Calculus in Scala"
organization               := "de.tu_dortmund.cs.ls14"
version                    := "1.0.0-SNAPSHOT"

scalaVersion               := "2.11.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"
resolvers += Resolver.mavenLocal
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3-SNAPSHOT"

import org.scalastyle.sbt.ScalastylePlugin
scalastyleConfig <<= baseDirectory / "lib/scalastyle_config.xml"

