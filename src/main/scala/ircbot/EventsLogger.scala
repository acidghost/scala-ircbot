package ircbot

import java.io.{FileWriter, PrintWriter}
import java.util.Calendar

import ircbot.Utils._


case class EventsLogger(basePath: String, receiver: String) {

    private val startedAt = Calendar.getInstance()

    // The second parameter enables appending instead of overwriting
    private val fw = new FileWriter(logFilename(basePath, startedAt, receiver), true)
    private val writer = new PrintWriter(fw)

    def apply(event: String): EventsLogger = {
        val logger = if (timeToChangeFile) {
                        writer.close()
                        EventsLogger(basePath, receiver)
                    } else this
        logger.writer.append(System.currentTimeMillis() + ":" + event + "\n")
        logger.writer.flush()
        logger
    }

    def close(): Unit = writer.close()

    private def timeToChangeFile =
        Calendar.getInstance().get(Calendar.DAY_OF_MONTH) != startedAt.get(Calendar.DAY_OF_MONTH)

}
