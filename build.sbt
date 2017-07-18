name := "ParScala"

version := "0.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % "2.12.2",
                            "org.scala-lang" % "scala-reflect" % "2.12.2",
                            "commons-cli" % "commons-cli" % "1.3.1",
                            "org.scalactic" %% "scalactic" % "3.0.0",
                            "org.scalatest" %% "scalatest" % "3.0.0" % "test",
                            "org.scalaz" %% "scalaz-core" % "7.2.8"
                            )

scalacOptions ++= Seq("-Xlint",
                      "-feature",
                      "-unchecked",
                      "-deprecation",
                      "-Ywarn-unused",
                      "-Ywarn-unused-import",
                      "-Ywarn-adapted-args",
                      "-target:jvm-1.8")

enablePlugins(JavaAppPackaging)

parallelExecution in Test := false
