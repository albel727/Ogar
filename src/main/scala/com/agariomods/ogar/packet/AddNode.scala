package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.ByteStringBuilder
import com.agariomods.ogar.entity.Cell

class AddNode(val item : Cell) extends Packet {
  def build() = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    // Only add player controlled cells with this packet or it will bug the camera
    val view = new ByteStringBuilder()

    view.putByte(32)
    view.putInt(this.item.nodeId)

    view.result()
  }
}

