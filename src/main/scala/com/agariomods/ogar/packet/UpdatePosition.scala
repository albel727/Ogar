package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

class UpdatePosition(x : Double, y : Double, size : Double) extends Packet {
  def build() = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    val view = new ByteStringBuilder

    view.putByte(17)
    view.putFloat(x.toFloat)
    view.putFloat(y.toFloat)
    view.putFloat(size.toFloat)

    view.result()
  }
}