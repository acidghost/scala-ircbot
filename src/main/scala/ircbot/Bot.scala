package ircbot

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging

import scala.collection.JavaConverters._
import scalaz.concurrent.Actor


case class Bot(host: String, port: Int, nick: String, channels: List[String]) extends StrictLogging {

    private val client = TCPClient(host, port)
    client write (Nick(nick), User(nick), Join(channels:_*))

    while (true)
        actor ! (client.read(): Message)


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
        case ServerMessage(msgNick, _, _, Nick(newNick)) =>
            logger.info(s"$msgNick is now known as $newNick")
        case _ => ()
    }

}

object Bot {

    def main(args: Array[String]): Unit = {
        val config = ConfigFactory.load("bot.conf")
        Bot(config.getString("ircbot.host"), config.getInt("ircbot.port"),
            config.getString("ircbot.nick"), config.getStringList("ircbot.channels").asScala.toList)
    }

}
