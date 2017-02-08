package ircbot

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Calendar


case class EventsLogger(basePath: String) {

    private val startedAt = Calendar.getInstance()

    private val file = new File(basePath + "/" + EventsLogger.formatCal(startedAt) + ".log")
    private val writer = new PrintWriter(file)

    def apply(event: String): EventsLogger = {
        val now = Calendar.getInstance()
        val logger = if (now.get(Calendar.DAY_OF_MONTH) != startedAt.get(Calendar.DAY_OF_MONTH)) {
                        writer.close()
                        EventsLogger(basePath)
                    } else this
        logger.writer.append(event + "\n")
        logger.writer.flush()
        logger
    }

    def close(): Unit = writer.close()

}

object EventsLogger {

    private val formatter = new SimpleDateFormat("yyyy-MM-dd")
    private def formatCal(calendar: Calendar) = formatter.format(calendar.getTime)

}
