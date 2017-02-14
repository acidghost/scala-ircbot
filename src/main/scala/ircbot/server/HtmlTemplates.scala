package ircbot.server

import java.util.Date

import ircbot.Utils._
import ircbot.server.Server.{LogLine, LogsReply}


private[server] object HtmlTemplates {

    import scalatags.Text.all._
    import scalatags.Text.tags2

    private def page(title: String, content: Seq[Modifier]): String =
        "<!DOCTYPE html>" +
        html(
            head(
                tags2.title(title)
            ),
            body()(content)
        ).toString

    def logs(channel: String, logsReply: LogsReply): String =
        page(s"Logs for $channel on ${logsReply.day}", Seq(
            h1(s"Logs for $channel on ${logsReply.day}"),
            ul(for (LogLine(d, sm) <- logsReply.data) yield {
                li(s"[${formatDateHuman(d)}] ${sm.humanLog}")
            })
        ))

}
