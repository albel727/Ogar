package com.agariomods.ogar

case class Color(r : Int, g : Int, b : Int) {
  def this(c : Color) = this(r = c.r, g = c.g, b = c.b)
}
