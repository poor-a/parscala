name := "ParScala"

version := "0.1"

scalaVersion := "2.12.6"

logLevel := Level.Info

libraryDependencies ++= Seq( "org.scala-lang" % "scala-compiler" % scalaVersion.value
                           , "org.scala-lang" % "scala-reflect" % scalaVersion.value
                           , "commons-cli" % "commons-cli" % "1.3.1"
                           , "org.scalactic" %% "scalactic" % "3.0.0"
                           , "org.scalatest" %% "scalatest" % "3.0.0" % "test"
                           , "org.scalaz" %% "scalaz-core" % "7.2.14"
                           , "org.scalameta" % "scalameta_2.12" % "3.7.4"
                           )

scalacOptions ++= Seq("-Xlint:_",
                      "-feature",
                      "-unchecked",
                      "-deprecation",
                      "-Xmaxerrs", "5",
                      "-Xmaxwarns", "5",
                      "-Ywarn-unused",
                      "-Ywarn-unused-import",
                      "-Ywarn-adapted-args",
                      "-opt:l:none",
                      "-target:jvm-1.8")

enablePlugins(JavaAppPackaging)

parallelExecution in Test := false
