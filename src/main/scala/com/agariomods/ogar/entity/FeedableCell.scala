package com.agariomods.ogar.entity

import com.agariomods.ogar.GameServer

trait FeedableCell {
  def feed(feeder : Cell, gameServer : GameServer) : Unit
}
