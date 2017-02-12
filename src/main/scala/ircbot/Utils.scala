package ircbot

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.language.implicitConversions


object Utils {

    implicit def CalendarToDate(calendar: Calendar): Date = calendar.getTime
    implicit def DateToCalendar(date: Date): Calendar = {
        val calendar = Calendar.getInstance()
        calendar.setTimeInMillis(date.getTime)
        calendar
    }

    private val formatter = new SimpleDateFormat("yyyy-MM-dd")
    def formatDate(date: Date): String = formatter.format(date)

    def logFilename(basePath: String, date: Date, receiver: String): String =
        s"$basePath/${formatDate(date)}-$receiver.log"

}
