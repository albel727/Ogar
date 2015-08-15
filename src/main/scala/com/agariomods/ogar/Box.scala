package com.agariomods.ogar

class Box(var bottomY : Int, var topY : Int, var rightX : Int, var leftX : Int) {
  def width = (rightX - leftX) / 2
  def height = (bottomY - topY) / 2
}
