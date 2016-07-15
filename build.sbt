name := "ParScala"

version := "0.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"

scalacOptions ++= Seq("-Xlint",
                      "-feature",
                      "-unchecked",
                      "-deprecation",
                      "-Ywarn-unused",
                      "-Ywarn-unused-import",
                      "-Ywarn-adapted-args",
                      "-target:jvm-1.8")
