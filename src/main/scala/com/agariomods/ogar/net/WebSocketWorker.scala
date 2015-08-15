package com.agariomods.ogar.net

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorRefFactory, Props}
import com.agariomods.ogar.packet.Packet
import com.agariomods.ogar.{GameServer, PacketHandler, PlayerTracker, ServerStats}
import spray.can.websocket.frame.{BinaryFrame, TextFrame}
import spray.can.websocket.{FrameCommandFailed, HandshakeContext, UpgradedToWebSocket}
import spray.can.{Http, websocket}
import spray.http.{AllOrigins, HttpHeaders, HttpOrigin, HttpRequest}
import spray.routing.HttpServiceActor

object WebSocketWorker {
  def props(remoteAddress : InetSocketAddress, serverConnection: ActorRef, server: GameServer) = Props(classOf[WebSocketWorker], remoteAddress, serverConnection, server)
}
class WebSocketWorker(_remoteAddress : InetSocketAddress, val serverConnection: ActorRef, val server: GameServer) extends HttpServiceActor with websocket.WebSocketServerWorker with OgarSocket {
  override def receive = originChecking orElse handshaking orElse businessLogicNoUpgrade orElse closeLogic

  override val remoteAddress = _remoteAddress.toString

  override val packetHandler : PacketHandler = new PacketHandler(server, WebSocketWorker.this)

  override val playerTracker : PlayerTracker = new PlayerTracker(server, WebSocketWorker.this)

  // Log connect event
  log.info("Connect: {}", remoteAddress)

  override def sendPacket(packet : Packet) = {
    if(null != packet) {
      log.debug("Sending websocket packet: {}", packet.getClass.getSimpleName)
      send(BinaryFrame(packet.build()))
    }
  }

  def originChecking : Receive = {
    case hr@websocket.HandshakeRequest(wsContext : HandshakeContext) =>
      // ----- Client authenticity check code -----
      // !!!!! WARNING !!!!!
      // THE BELOW SECTION OF CODE CHECKS TO ENSURE THAT CONNECTIONS ARE COMING
      // FROM THE OFFICIAL AGAR.IO CLIENT. IF YOU REMOVE OR MODIFY THE BELOW
      // SECTION OF CODE TO ALLOW CONNECTIONS FROM A CLIENT ON A DIFFERENT DOMAIN,
      // YOU MAY BE COMMITTING COPYRIGHT INFRINGEMENT AND LEGAL ACTION MAY BE TAKEN
      // AGAINST YOU. THIS SECTION OF CODE WAS ADDED ON JULY 9, 2015 AT THE REQUEST
      // OF THE AGAR.IO DEVELOPERS.
      val origins = wsContext.request.header[HttpHeaders.Origin].fold(Seq[HttpOrigin]())(_.originList)
      if(!origins.exists(origin => origin.host.host == "localhost" || origin.host.host == "agar.io")) {
        serverConnection ! Http.Abort
        if(log.isInfoEnabled) {
          log.info("Declined unofficial client connection. Origin: {}", origins.mkString(" "))
        }
      }
      // -----/Client authenticity check code -----
      handshaking(hr)
  }

  def businessLogic: Receive = {
    case x : BinaryFrame =>
      server.synchronized {
        packetHandler.handleMessage(x.payload)
      }

    case x : TextFrame =>
      log.error("Received unknown text frame {}", x)

    case x: FrameCommandFailed =>
      log.error("Frame command failed {}", x)

    case x: HttpRequest => // do something
      log.error("Received HttpRequest for some reason: {}", x)

    case UpgradedToWebSocket =>
      if(context.children.size >= server.config.serverMaxConnections) {
        log.info("Exceeded serverMaxConnections. Terminating incoming websocket connection from: {}", remoteAddress)
        serverConnection ! Http.Abort
      } else {
        log.info("Accepting incoming websocket connection from: {}", remoteAddress)
        server.clients += this
      }
    case x : Http.ConnectionClosed =>
      // Log disconnections
      log.info("Disconnect: {}", remoteAddress)
      closeLogic.apply(x) // Stops actor and logs more details
  }

  override def postStop() = {
    close()
    super.postStop()
  }

  def close() {
    val client = this.playerTracker

    client.cells.foreach(cell => {
      if (null != cell) {
        /* UNPORTED
        cell.calcMove = function() {return;}; // Clear function so that the cell cant move
        */

        //this.server.removeNode(cell);
      }
    })

    client.disconnect = this.server.config.playerDisconnectTime * 20

    /* UNPORTED
    this.socket.sendPacket = function() {return;}; // Clear function so no packets are sent
    */
  }

  def businessLogicNoUpgrade: Receive = {
    import com.agariomods.ogar.net.ServerStatsJsonSupport._

    implicit val refFactory: ActorRefFactory = context
    val decompressCompress = decompressRequest() & compressResponseIfRequested(())
    runRoute {
      decompressCompress {
        pathEndOrSingleSlash {
          getFromResource("webapp/index.html")
        } ~
        path("stats") {
          respondWithHeader(HttpHeaders.`Access-Control-Allow-Origin`(AllOrigins)) {
            produce(instanceOf[ServerStats]) {
              completionFunction => ctx => completionFunction(server.getStats)
            }
          }
        } ~
        getFromResourceDirectory("webapp")
      }
    }
  }
}
