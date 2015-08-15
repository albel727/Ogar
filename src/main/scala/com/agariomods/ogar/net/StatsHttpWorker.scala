package com.agariomods.ogar.net

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorRefFactory, Props}
import com.agariomods.ogar.{GameServer, ServerStats}
import spray.http.{AllOrigins, HttpHeaders}
import spray.routing.HttpServiceActor

object StatsHttpWorker {
  def props(remoteAddress : InetSocketAddress, serverConnection: ActorRef, server: GameServer) = Props(classOf[StatsHttpWorker], remoteAddress, serverConnection, server)
}
class StatsHttpWorker(_remoteAddress : InetSocketAddress, val serverConnection: ActorRef, val server: GameServer) extends HttpServiceActor {
  override def receive : Receive = {
    import com.agariomods.ogar.net.ServerStatsJsonSupport._

    implicit val refFactory: ActorRefFactory = context
    val decompressCompress = decompressRequest() & compressResponseIfRequested(())

    runRoute {
      get {
        decompressCompress {
          respondWithHeader(HttpHeaders.`Access-Control-Allow-Origin`(AllOrigins)) {
            produce(instanceOf[ServerStats]) {
              completionFunction => ctx => completionFunction(server.getStats)
            }
          }
        }
      }
    }
  }
}
