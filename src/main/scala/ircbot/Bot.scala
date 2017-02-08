package ircbot

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging
import io.circe.syntax._

import scala.collection.JavaConverters._
import scalaz.concurrent.Actor


case class Bot(host: String, port: Int, nick: String, channels: Seq[String], logsPath: String) extends StrictLogging {


    private val client = TCPClient(host, port)
    client write (Nick(nick), User(nick), Join(channels:_*))

    loop(EventsLogger(logsPath))


    private def loop(evtLogger: EventsLogger): Unit = {
        val msg = client.read(): Message
        actor ! msg
        loop (msg match {
            case sm: ServerMessage if sm.msg.isInstanceOf[Join]
                                   |  sm.msg.isInstanceOf[Part]
                                   |  sm.msg.isInstanceOf[Part]
                                   |  sm.msg.isInstanceOf[PrivMsg]
                                   |  sm.msg.isInstanceOf[Nick]     => evtLogger(sm.asJson.noSpaces)
            case _                                                  => evtLogger
        })
    }


    lazy private val actor = Actor[Message] {
        case Ping(servers) =>
            client.write(Pong(servers))

        case ServerMessage(msgNick, _, _, Join(msgChannels @ _*)) =>
            logger.info(s"$msgNick joined ${msgChannels.mkString(", ")}")

        case ServerMessage(msgNick, _, _, Part(msgChannels, reason)) =>
            logger.info(s"$msgNick parted ${msgChannels.mkString(", ")}" + (reason match {
                case Some(r) => " [" + r + "]"
                case None => ""
            }))

        case ServerMessage(msgNick, _, _, PrivMsg(targets, text)) =>
            logger.info(s"$msgNick in ${targets.mkString(", ")}: $text")
            if (targets.contains(nick)) {
                val cmd :: _ = text.split(" ").toList
                val reply = cmd match {
                    case "help" => "This is still a test..."
                    case _      => "Unrecognized command: " + cmd
                }
                client.write(PrivMsg(Seq(msgNick), reply))
            }

        case ServerMessage(msgNick, _, _, Nick(newNick)) =>
            logger.info(s"$msgNick is now known as $newNick")

        case _ => ()
    }

}

object Bot {

    def main(args: Array[String]): Unit = {
        val botConf = ConfigFactory.load("bot.conf").getConfig("ircbot")
        Bot(botConf.getString("host"), botConf.getInt("port"),
            botConf.getString("nick"), botConf.getStringList("channels").asScala,
            botConf.getString("logsPath"))
    }

}
