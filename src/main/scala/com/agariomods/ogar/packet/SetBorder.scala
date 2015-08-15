package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

class SetBorder(left : Double, right : Double, top : Double, bottom : Double) extends Packet {
  def build() = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    val view = new ByteStringBuilder

    view.putByte(64)
    view.putDouble(this.left)
    view.putDouble(this.top)
    view.putDouble(this.right)
    view.putDouble(this.bottom)

    view.result()
  }
}