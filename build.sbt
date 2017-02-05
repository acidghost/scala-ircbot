name := "scala-ircbot"

version := "1.0"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.8"
val slf4jVersion = "1.7.22"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
    "com.typesafe" % "config" % "1.3.1",
    "org.slf4j" % "slf4j-api" % slf4jVersion,
    "org.slf4j" % "slf4j-log4j12" % slf4jVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "com.typesafe.play" %% "play-json" % "2.5.12"
)
