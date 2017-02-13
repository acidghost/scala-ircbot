package ircbot.server

import java.util.{Calendar, Date}

import com.twitter.finagle.Http
import com.twitter.finagle.http.Response
import com.twitter.io.Buf
import com.twitter.util.Await
import com.typesafe.config.ConfigFactory
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Encoder, Json}
import io.finch._
import io.finch.circe._
import ircbot.Utils.formatDateHuman
import ircbot.server.{HtmlTemplates => html}
import ircbot.{LogParser, ServerMessage}


object Server {

    implicit val dateEncoder: Encoder[Date] = new Encoder[Date] {
        override def apply(date: Date): Json = formatDateHuman(date).asJson
    }

    case class LogLine(date: Date, message: ServerMessage)
    case class LogsReply(data: List[LogLine])

    private val config = ConfigFactory.load("bot.conf").getConfig("ircbot")
    private val logsPath = config.getString("logsPath")

    private def getLogs(channel: String, days: Option[Int]): Option[(Date, LogsReply)] = {
        val date = days match {
            case Some(d) =>
                val c = Calendar.getInstance()
                c.add(Calendar.DATE, -d)
                c.getTime
            case None => new Date()
        }

        LogParser(channel, date, logsPath) match {
            case logs if logs.isEmpty  => None
            case logs                  => Some((date, LogsReply(logs.map { case (d, m) => LogLine(d, m) }.toList)))
        }
    }

    private def htmlResponse(document: String): Response = {
        val rep = Response()
        rep.content = Buf.Utf8(document)
        rep.contentType = "text/html"
        rep
    }

    private val logsHtmlEndpoint: Endpoint[Response] =
        get("logs" :: "html" :: string("channel") :: paramOption("days").as[Int]) { (channel: String, days: Option[Int]) =>
            getLogs('#' + channel, days) match {
                case Some((date, logs)) => Ok(htmlResponse(html.logs('#' + channel, date, logs)))
                case None               => NotFound(new Exception("No logs available..."))
            }
        }

    private val logsJsonEndpoint: Endpoint[LogsReply] =
        get("logs" :: "json" :: string("channel") :: paramOption("days").as[Int]) { (channel: String, days: Option[Int]) =>
            getLogs('#' + channel, days) match {
                case Some((_, logs)) => Ok(logs)
                case None            => NotFound(new Exception("No logs available..."))
            }
        }

    private val api = logsJsonEndpoint :+: logsHtmlEndpoint

    def start(): Unit = Await.ready(Http.server.serve(s":${config.getInt("server.port")}", api.toService))

    def main(args: Array[String]): Unit = Server.start()

}
