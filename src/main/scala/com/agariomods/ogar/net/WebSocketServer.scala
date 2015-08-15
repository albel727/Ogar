package com.agariomods.ogar.net

import akka.actor.{Actor, ActorLogging, Props}
import com.agariomods.ogar.GameServer
import spray.can.Http

object WebSocketServer {
  def props(gameServer: GameServer) = Props(classOf[WebSocketServer], gameServer)
}
class WebSocketServer(gameServer: GameServer) extends Actor with ActorLogging {
  def receive = {
    case Http.Connected(remoteAddress, localAddress) =>
      // when a new connection comes in we register a WebSocketWorker actor as the per connection handler
      val serverConnection = sender()
      val conn = context.actorOf(WebSocketWorker.props(remoteAddress, serverConnection, gameServer))
      serverConnection ! Http.Register(conn)
    case e =>
      log.error("Unknown message type: {}", e)
  }
}

