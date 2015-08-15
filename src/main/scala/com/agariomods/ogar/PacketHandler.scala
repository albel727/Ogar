package com.agariomods.ogar

import java.nio.ByteOrder

import akka.util.{ByteIterator, ByteString}
import com.agariomods.ogar.entity.Cell
import com.agariomods.ogar.net.OgarSocket
import com.agariomods.ogar.packet.SetBorder
import org.slf4j.LoggerFactory

import scala.collection.mutable

object PacketHandler {
  val logger = LoggerFactory.getLogger(classOf[PacketHandler])
}

class PacketHandler(var gameServer : GameServer, var socket : OgarSocket) {
  // Detect protocol version - we can do something about it later
  var protocol = 0

  var pressQ = false
  var pressW = false
  var pressSpace = false

  val multiCells = mutable.HashSet.empty[Cell]

  def parseNullString16(packet: ByteIterator, maxLength : Int = Int.MaxValue) = new String(Iterator.continually(packet.getShort(ByteOrder.LITTLE_ENDIAN)).takeWhile(_ != 0).take(maxLength).map(_.toChar).toArray)

  def handleMessage(message : ByteString) {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    // Discard empty messages
    if (message.size == 0) {
      return
    }

    val view = message.iterator
    val packetId = view.getByte

    packetId match {
      case 0 =>
        // Check for invalid packets
        if ((message.size + 1) % 2 == 1) {
          return
        }

        // Set Nickname
        val maxLen = Math.min(this.gameServer.config.playerMaxNickLength, (message.size - 1) / 2)
        val nick = parseNullString16(view, maxLen)
        this.setNickname(nick)

        if(PacketHandler.logger.isDebugEnabled) {
          PacketHandler.logger.debug("{}", s"${socket.remoteAddress} in: nick $nick")
        }
      case 1 =>
        // Spectate mode
        if (this.socket.playerTracker.cells.size <= 0) {
          // Make sure client has no cells
          this.gameServer.switchSpectator(this.socket.playerTracker)
          this.socket.playerTracker.spectate = true
        }

        PacketHandler.logger.debug("{} in: spectate", socket.remoteAddress)
      case 16 =>
        // Set Target
        // Discard broken packets, handle float64s or int16s depending on how Zeach feels at the time
        val client = this.socket.playerTracker
        val (newX, newY, nodeId) =
        if (message.size == 9) {
          (view.getShort.toDouble, view.getShort.toDouble, view.getInt)
        } else if (message.size == 13) {
          (view.getInt.toDouble, view.getInt.toDouble, view.getInt)
        } else if (message.size == 21) {
          (view.getDouble, view.getDouble, view.getInt)
        } else {
          return
        }

        val pos = Position.toInt(x = newX, y = newY)
        if (nodeId == 0) {
          client.mouseCells.clear() // Disable individual cell movement
          client.mouse = pos
        } else {
          client.mouseCells(nodeId) = pos
        }

        if(PacketHandler.logger.isDebugEnabled) {
          PacketHandler.logger.debug("{}", s"${socket.remoteAddress} in: target $pos for blob $nodeId")
        }
      case 17 =>
        // Space Press - Split cell
        this.pressSpace = true

        PacketHandler.logger.debug("{} in: split", socket.remoteAddress)
      case 18 =>
        // Q Key Pressed
        this.pressQ = true

        PacketHandler.logger.debug("{} in: q on", socket.remoteAddress)
      case 19 =>
        // Q Key Released

        PacketHandler.logger.debug("{} in: q off", socket.remoteAddress)
      case 21 =>
        // W Press - Eject mass
        this.pressW = true

        PacketHandler.logger.debug("{} in: eject", socket.remoteAddress)
      case -1 => //255
        // Connection Start
        if (message.size == 5) {
          this.protocol = view.getInt
          // Send SetBorder packet first
          val c = this.gameServer.config
          this.socket.sendPacket(new SetBorder(c.borderLeft, c.borderRight, c.borderTop, c.borderBottom))
        }

        PacketHandler.logger.debug("{} in: connection start", socket.remoteAddress)
      case _ =>
    }
  }

  def setNickname(newNick : String) {
    val client = this.socket.playerTracker
    if (client.cells.size < 1) {
      // Set name first
      client.setName(newNick)

      // If client has no cells... then spawn a player
      this.gameServer.gameMode.onPlayerSpawn(this.gameServer,client)

      // Turn off spectate mode
      client.spectate = false
    }
  }
}
