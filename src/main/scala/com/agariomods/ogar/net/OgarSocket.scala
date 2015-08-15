package com.agariomods.ogar.net

import com.agariomods.ogar.packet.Packet
import com.agariomods.ogar.{PacketHandler, PlayerTracker}

trait OgarSocket {
  def remoteAddress : String
  def packetHandler : PacketHandler
  def playerTracker: PlayerTracker
  def sendPacket(packet : Packet)
  def close()
}
