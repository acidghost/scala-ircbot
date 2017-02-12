package ircbot

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging
import io.circe.syntax._

import scala.annotation.tailrec
import scala.collection.JavaConverters._


case class Bot(host: String, port: Int, nick: String, channels: Seq[String], logsPath: String) extends StrictLogging {

    private type EventsLoggers = Map[String, EventsLogger]
    // Maps channels to users
    private type UsersMap = Map[String, Set[String]]

    private val client = TCPClient(host, port)
    client write (Nick(nick), User(nick), Join(channels:_*))

    // Wait for names reply and get users for each channel
    private val users = waitForNamesReplies(
        channels.map(channel => channel -> false).toMap,
        channels.map(channel => channel -> Set.empty[String]).toMap)

    // Then start the main loop
    loop(channels.map(channel => channel -> EventsLogger(logsPath, channel)).toMap, users)


    @tailrec
    private def waitForNamesReplies(channelsDone: Map[String, Boolean], users: UsersMap): UsersMap = {
        if (channelsDone.forall(_._2))
            return users

        client.read() match {
            case Some(line) =>
                (line: Message) match {
                    case ServerMessage(_, _, _, NamesReply(_, channel, nicks)) =>
                        logger.info(s"New names for $channel: ${nicks.mkString(", ")}")
                        val newUsers = users.updated(channel, users(channel) ++ nicks.toSet)
                        waitForNamesReplies(channelsDone, newUsers)
                    case ServerMessage(_, _, _, NamesEndReply(channel)) =>
                        logger.info(s"End of names for $channel [${users(channel).size}]")
                        waitForNamesReplies(channelsDone.updated(channel, true), users)
                    case _ => waitForNamesReplies(channelsDone, users)
                }
            case None => waitForNamesReplies(channelsDone, users)
        }
    }


    @tailrec
    private def loop(evtLoggers: EventsLoggers, users: UsersMap): Unit = client.read() match {
        case Some(line) =>
            val msg = line: Message
            unitActor(msg)
            val newUsers = updateUsers(users, msg)
            loop(logMessage(evtLoggers, newUsers, msg), newUsers)
        case None =>
            loop(evtLoggers, users)
    }


    private def unitActor: PartialFunction[Message, Unit] = {
        case Ping(servers)                         => client.write(Pong(servers))
        case ServerMessage(_, _, _, Ping(servers)) => client.write(Pong(servers))

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


    private def channelsForUser(nk: String, usersMap: UsersMap): List[String] =
        channels.filter(chan => usersMap(chan).contains(nk)).toList


    private def updateUsers(users: UsersMap, message: Message): UsersMap = message match {
        case ServerMessage(nk, _, _, Join(chans @ _*)) =>
            addUser(chans, users, nk)
        case ServerMessage(nk, _, _, Part(chans, _)) =>
            delUser(chans, users, nk)
        case ServerMessage(nk, _, _, Quit(_)) =>
            delUser(channelsForUser(nk, users), users, nk)
        case ServerMessage(oldNk, _, _, Nick(newNk)) =>
            val chans = channelsForUser(oldNk, users)
            addUser(chans, delUser(chans, users, oldNk), newNk)
        case _ => users
    }

    private def addUser(cs: Seq[String], usersMap: UsersMap, nk: String): UsersMap =
        if (cs.isEmpty) usersMap else addUser(cs.tail, usersMap.updated(cs.head, usersMap(cs.head) + nk), nk)
    private def delUser(cs: Seq[String], usersMap: UsersMap, nk: String): UsersMap =
        if (cs.isEmpty) usersMap else delUser(cs.tail, usersMap.updated(cs.head, usersMap(cs.head) - nk), nk)


    private def logMessage(evtLoggers: EventsLoggers, users: UsersMap, message: Message): EventsLoggers = message match {
        case sm @ ServerMessage(_, _, _, Join(chans @ _*)) =>
            logMessage(chans.toList, evtLoggers, sm)
        case sm @ ServerMessage(_, _, _, Part(chans, _)) =>
            logMessage(chans.toList, evtLoggers, sm)
        case sm @ ServerMessage(_, _, _, PrivMsg(receivers, _)) =>
            logMessage(receivers.toList, evtLoggers, sm)
        case sm @ ServerMessage(_, _, _, Nick(nk)) =>
            logMessage(channelsForUser(nk, users), evtLoggers, sm)
        case sm @ ServerMessage(nk, _, _, Quit(_)) =>
            logMessage(channelsForUser(nk, users), evtLoggers, sm)
        case _ => evtLoggers
    }

    private def logMessage(chans: List[String], evtLoggers: EventsLoggers, message: ServerMessage): EventsLoggers = {
        if (chans.isEmpty)
            return evtLoggers
        val chan :: left = chans
        if (evtLoggers.contains(chan))
            logMessage(left, evtLoggers.updated(chan, evtLoggers(chan)(message.asJson.noSpaces)), message)
        else
            logMessage(left, evtLoggers, message)
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
