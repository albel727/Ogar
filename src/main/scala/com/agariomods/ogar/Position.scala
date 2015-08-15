package com.agariomods.ogar

case class Position(x : Int, y : Int)

object Position {
  def toInt(x : Double, y : Double) = new Position(x.toInt, y.toInt)
  val Zero = new Position(x = 0, y = 0)
}