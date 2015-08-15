package com.agariomods.ogar.net

import akka.actor.{Props, Actor, ActorLogging}
import com.agariomods.ogar.GameServer
import spray.can.Http

object StatsHttpServer {
  def props(gameServer: GameServer) = Props(classOf[StatsHttpServer], gameServer)
}
class StatsHttpServer(gameServer: GameServer) extends Actor with ActorLogging {
  def receive = {
    case Http.Connected(remoteAddress, localAddress) =>
      // when a new connection comes in we register a StatsHttpWorker actor as the per connection handler
      val serverConnection = sender()
      val conn = context.actorOf(StatsHttpWorker.props(remoteAddress, serverConnection, gameServer))
      serverConnection ! Http.Register(conn)
    case e =>
      log.error("Unknown message type: {}", e)
  }
}

