package ircbot


import io.circe._
// NOTE don't remove the following line!
import io.circe.generic.auto._
import io.circe.generic.semiauto._

import scala.language.implicitConversions


sealed trait Message {
    def encode: String
}

private case class UnrecognizedMessage(raw: String) extends Message {
    override def encode: String = throw new Exception("Non-protocol message")
}

case class Ping(servers: String*) extends Message {
    override def encode: String = s"PING :${servers.mkString(",")}"
}

case class Pong(servers: String*) extends Message {
    override def encode: String = s"PONG :${servers.mkString(",")}"
}

case class Nick(nick: String) extends Message {
    override def encode: String = s"NICK :$nick"
}

case class User(user: String, real: Option[String] = None) extends Message {
    override def encode: String = s"USER $user 0 * :${real.getOrElse(user)}"
}

case class Join(channels: String*) extends Message {
    override def encode: String = s"JOIN ${channels.mkString(",")}"
}

case class Part(channels: Seq[String], reason: Option[String] = None) extends Message {
    override def encode: String = "PART " + channels.mkString(",") + (reason match {
        case Some(r) => " :" + r
        case _       => ""
    })
}

case class PrivMsg(receivers: Seq[String], text: String) extends Message {
    override def encode: String = s"PRIVMSG ${receivers.mkString(",")} :$text"
}

case class ServerMessage(nick: String, name: Option[String], host: Option[String], msg: Message) extends Message {
    override def encode: String = {
        var out = ":%s".format(nick)
        name foreach { n => out = "%s!%s".format(out, n) }
        host foreach { h => out = "%s@%s".format(out, h) }
        "%s %s".format(out, msg.encode)
    }
}


object Message {

    implicit def MessageToString(message: Message): String = message.encode

    private val decoders = Map[String, PartialFunction[List[String], Message]](
        "PING" -> {
            case servers :: Nil => Ping(servers.split(","):_*)
        },
        "PONG" -> {
            case servers :: Nil => Pong(servers.split(","):_*)
        },
        "NICK" -> {
            case nick :: Nil => Nick(nick)
        },
        "JOIN" -> {
            case channels :: Nil => Join(channels.split(","):_*)
        },
        "PART" -> {
            case channels :: Nil => Part(channels.split(","))
            case channels :: reason :: Nil => Part(channels.split(","), Some(reason))
        },
        "PRIVMSG" -> {
            case targets :: text :: Nil => PrivMsg(targets.split(",").toList, text)
        }
    )

    private val ServerUser = """([^!]+)!?([^@]*)?@?(.+)?""".r

    implicit def StringToMsg(string: String): Message = {
        def decodeCmd(cmdStr: String) = {
            val cmd :: body :: Nil = cmdStr.split(" ", 2).toList
            val cmdName = cmd.toUpperCase
            val bodyArgs = body.indexOf(":") match {
                case -1 => body.split(" ").toList
                case i  => body.substring(0, i).split(" ").toList :+ body.substring(i+1)
            }

            decoders get cmdName flatMap { _.lift(bodyArgs) } getOrElse UnrecognizedMessage(cmdStr)
        }

        try {
            if (string.head == ':') {
                val head :: cmd = string.tail.split(" ").toList
                val ServerUser(nick, name, host) = head
                ServerMessage(nick, Option(name), Option(host), decodeCmd(cmd.mkString(" ")))
            } else decodeCmd(string)
        } catch {
            case _: Throwable => UnrecognizedMessage(string)
        }
    }

    // Could they be mandatory because they ServerMessage contains a reference to a trait (Message)?
    implicit val smDecoder: Decoder[ServerMessage] = deriveDecoder
    implicit val smEncoder: Encoder[ServerMessage] = deriveEncoder

}
