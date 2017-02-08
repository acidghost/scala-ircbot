name := "scala-ircbot"

version := "1.0"

scalaVersion := "2.11.8"

val Versions = new Object {
    val circe = "0.7.0"
    val scalaz = "7.2.8"
    val slf4j = "1.7.22"
}

val scalaz = Seq(
    "core", "concurrent"
).map(x => "org.scalaz" %% s"scalaz-$x" % Versions.scalaz)

val circe = Seq(
    "core", "generic", "parser"
).map(x => "io.circe" %% s"circe-$x" % Versions.circe)

val logging = Seq(
    "org.slf4j" % "slf4j-api" % Versions.slf4j,
    "org.slf4j" % "slf4j-log4j12" % Versions.slf4j,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
)

libraryDependencies ++= scalaz ++
                        circe ++
                        logging ++
                        Seq(
                            "com.typesafe" % "config" % "1.3.1"
                        )
