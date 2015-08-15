package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

class DrawLine(x : Int, y : Int) extends Packet {
  def build() = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    val view = new ByteStringBuilder

    view.putByte(21)
    view.putShort(this.x)
    view.putShort(this.y)

    view.result()
  }
}