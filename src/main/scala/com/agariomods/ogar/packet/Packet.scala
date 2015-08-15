package com.agariomods.ogar.packet

import akka.util.ByteString

trait Packet {
  def build() : ByteString
}
