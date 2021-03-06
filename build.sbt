name := "scala-ircbot"

version := "1.0"

scalaVersion := "2.11.8"

val Versions = new Object {
    val circe = "0.7.0"
    val slf4j = "1.7.22"
    val finch = "0.13.0"
}

val circe = Seq(
    "core", "generic", "parser"
).map(x => "io.circe" %% s"circe-$x" % Versions.circe)

val logging = Seq(
    "org.slf4j" % "slf4j-api" % Versions.slf4j,
    "org.slf4j" % "slf4j-log4j12" % Versions.slf4j,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
)

val finch = Seq(
    "core", "circe"
).map(x => "com.github.finagle" %% s"finch-$x" % Versions.finch)

libraryDependencies ++= circe ++
                        logging ++
                        finch ++
                        Seq(
                            "com.typesafe" % "config" % "1.3.1",
                            "com.lihaoyi" %% "scalatags" % "0.6.3"
                        )
