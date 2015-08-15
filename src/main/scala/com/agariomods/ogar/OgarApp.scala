package com.agariomods.ogar

import java.io.PrintWriter
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem, Inbox}
import akka.io.IO
import com.agariomods.ogar.modules.IniReflect
import com.agariomods.ogar.net.{StatsHttpServer, WebSocketServer}
import jline.console.ConsoleReader
import jline.console.completer.{ArgumentCompleter, NullCompleter, StringsCompleter}
import org.slf4j.LoggerFactory
import spray.can.Http
import spray.can.Http.Bind
import spray.can.server.UHttp

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

class OgarConsole // Just a marker class for console log output

object OgarApp {
  val consoleLogger = LoggerFactory.getLogger(classOf[OgarConsole])

  def main(args : Array[String]) {
    val reader = new ConsoleReader
    implicit val out = new PrintWriter(reader.getOutput, true)

    out.println("[Game] Ogar - An open source Agar.io server implementation")

    implicit val system = ActorSystem()

    val gameServer = new GameServer

    gameServer.commands.loadConfig(gameServer, out)

    gameServer.start(system)

    out.println("[Game] Current game mode is " + gameServer.gameMode.name)

    val timeout = FiniteDuration(10, TimeUnit.SECONDS)

    out.println(s"[Game] Starting game server on port ${gameServer.config.serverPort}...")
    val websocketServer = system.actorOf(WebSocketServer.props(gameServer), "GameWebsocketServer")
    tryBinding(websocketServer, timeout, Http.Bind(websocketServer, gameServer.config.serverInterface, gameServer.config.serverPort), "serverPort")

    gameServer.config.serverStatsPort match {
      case x if (x <= 0) || (x == gameServer.config.serverPort) =>
        // Don't start separate stats server,
        // websocketServer always handles stat requests on /stats path anyway
      case x =>
        // Try starting an additional http stats server handling stats requests on any path
        out.println(s"[Game] Starting stats server on port $x...")
        val statsServer = system.actorOf(StatsHttpServer.props(gameServer), "StatsHttpServer")
        tryBinding(statsServer, timeout, Http.Bind(statsServer, gameServer.config.serverInterface, x), "serverStatsPort")
    }

    import scala.collection.JavaConverters._

    reader.setPrompt(">")
    reader.addCompleter(
      new ArgumentCompleter(
        new StringsCompleter(gameServer.commands.list.keySet.asJava),
        new StringsCompleter(IniReflect.listConfigKeys(gameServer.config).asJavaCollection),
        NullCompleter.INSTANCE
      )
    )

    var keepGoing = true
    while (keepGoing) {
      val str = reader.readLine(">")

      // Log the string
      consoleLogger.info("{}", str)

      // Don't process ENTER
      if (!str.isEmpty) {
        // Splits the string
        val split = str.split(' ')

        // Process the first string value
        val first = split.lift(0).map(_.toLowerCase)
        if(first.contains("exit")) {
          keepGoing = false // Stop
        } else {
          // Get command function
          val execute = first.flatMap(gameServer.commands.list.get)
          execute.map(_(gameServer, split, reader)).getOrElse(out.println("[Console] Invalid Command!"))
        }
      }
    }

    system.shutdown()
    system.awaitTermination()
  }

  def tryBinding(server : ActorRef, timeout : FiniteDuration, bind : Bind, serverPortOption : String)(implicit system : ActorSystem, out : PrintWriter) {
    val inbox = Inbox.create(system)
    inbox.send(IO(UHttp), bind)
    val response = Try {
      inbox.receive(timeout)
    }
    response match {
      case Success(Http.Bound(localAddress)) =>
        out.println(s"[Game] Listening on $localAddress")
      case Success(Http.CommandFailed(b : Bind)) if b.listener == server =>
        out.println(s"[Error] Bind to ${b.endpoint} failed.")
        out.println(s"[Error] Please close out of Skype or change '$serverPortOption' in gameserver.ini to a different number.")
        if(b.endpoint.getPort < 1024) {
          out.println("[Error] Since your configured port is less than 1024, please also make sure you are running Ogar with root privileges.")
        }
        System.exit(1)
      case Failure(_) =>
        out.println(s"[Error] Failed to bind in $timeout")
        System.exit(2)
    }
  }
}