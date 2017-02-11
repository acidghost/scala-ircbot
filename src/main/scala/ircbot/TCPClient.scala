package ircbot

import java.io.{BufferedReader, DataOutputStream, InputStreamReader}
import java.net.Socket


case class TCPClient(host: String, port: Int, delimiter: String = "\r\n") {

    private val socket = new Socket(host, port)
    private val out = new DataOutputStream(socket.getOutputStream)
    private val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

    def read(): Option[String] = Option(in.readLine())

    def write(msgs: String*): Unit = msgs foreach (msg => out writeBytes (msg + delimiter))

}
