package com.agariomods.ogar.entity

trait SpeedableCell extends Cell {
  var speedMultiplier = 1d

  override def getSpeed : Double = super.getSpeed * this.speedMultiplier
}
