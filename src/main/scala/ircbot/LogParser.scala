package ircbot

import java.io.{BufferedReader, File, FileReader}
import java.util.Date

import io.circe.parser._
import ircbot.Utils._

import scala.util.Try


object LogParser {

    def apply(receiver: String, date: Date, logsPath: String): Map[Date, ServerMessage] = {
        val file = new File(logFilename(logsPath, date, receiver))
        val readerTry = Try(new BufferedReader(new FileReader(file)))
        if (readerTry.isFailure)
            return Map.empty[Date, ServerMessage]
        val reader = readerTry.get

        def rebuildLog(messages: Map[Date, ServerMessage]): Map[Date, ServerMessage] = {
            val line = reader.readLine()
            if (line == null) {
                reader.close()
                return messages
            }

            val time :: msgStr :: Nil = line.split(":", 2).toList
            val msgDate = new Date(time.toLong)

            decode[ServerMessage](msgStr) match {
                case Left(_) =>
                    // TODO how to notify failure? Just skip for now
                    rebuildLog(messages)
                case Right(msg) =>
                    rebuildLog(messages + (msgDate -> msg))
            }
        }

        rebuildLog(Map.empty[Date, ServerMessage])
    }

}
