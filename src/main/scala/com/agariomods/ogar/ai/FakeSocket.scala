package com.agariomods.ogar.ai

import com.agariomods.ogar.net.OgarSocket
import com.agariomods.ogar.packet.Packet
import com.agariomods.ogar.{GameServer, PacketHandler, PlayerTracker}

// A fake socket for bot players

class FakeSocket(val server : GameServer) extends OgarSocket {
  override def remoteAddress = "BOT"

  override val packetHandler : PacketHandler = new PacketHandler(server, FakeSocket.this)

  override val playerTracker : PlayerTracker = new BotPlayer(server, FakeSocket.this)

  // Add to client list
  server.clients += this

  override def sendPacket(packet : Packet) = {
    // Fakes sending a packet
  }

  override def close() {
    // Removes the bot
    while(this.playerTracker.cells.nonEmpty) {
      this.server.removeNode(this.playerTracker.cells.head)
    }

    this.server.clients -= this
  }
}
