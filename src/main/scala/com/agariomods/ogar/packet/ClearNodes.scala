package com.agariomods.ogar.packet

import akka.util.ByteStringBuilder

class ClearNodes() extends Packet {
  def build() = {
    val view = new ByteStringBuilder()

    view.putByte(20)

    view.result()
  }
}