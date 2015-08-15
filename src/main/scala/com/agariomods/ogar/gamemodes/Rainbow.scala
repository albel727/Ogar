package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.{Cell, Food}
import com.agariomods.ogar.{Color, GameServer, PlayerTracker, Position}

trait RainbowCell {
  var rainbow : Int = -1
}

class RainbowFood(
                   _nodeId : Int,
                   _owner : Option[PlayerTracker], // playerTracker that owns this cell
                   _position : Position,
                   _mass : Double,
                   _gameServer : GameServer
                   ) extends Food(_nodeId, _owner, _position, _mass, _gameServer) {
  override def sendUpdate() = true
}

class Rainbow extends FFA {

  override val ID = 20
  override val name = "Rainbow FFA"
  override val specByLeaderboard = true

  val colors = Array(
    new Color(r = 255, g = 0, b = 0), // Red
    new Color(r = 255, g = 32, b = 0),
    new Color(r = 255, g = 64, b = 0),
    new Color(r = 255, g = 96, b = 0),
    new Color(r = 255, g = 128, b = 0), // Orange
    new Color(r = 255, g = 160, b = 0),
    new Color(r = 255, g = 192, b = 0),
    new Color(r = 255, g = 224, b = 0),
    new Color(r = 255, g = 255, b = 0), // Yellow
    new Color(r = 192, g = 255, b = 0),
    new Color(r = 128, g = 255, b = 0),
    new Color(r = 64, g = 255, b = 0),
    new Color(r = 0, g = 255, b = 0), // Green
    new Color(r = 0, g = 192, b = 64),
    new Color(r = 0, g = 128, b = 128),
    new Color(r = 0, g = 64, b = 192),
    new Color(r = 0, g = 0, b = 255), // Blue
    new Color(r = 18, g = 0, b = 192),
    new Color(r = 37, g = 0, b = 128),
    new Color(r = 56, g = 0, b = 64),
    new Color(r = 75, g = 0, b = 130), // Indigo
    new Color(r = 92, g = 0, b = 161),
    new Color(r = 109, g = 0, b = 192),
    new Color(r = 126, g = 0, b = 223),
    new Color(r = 143, g = 0, b = 255), // Purple
    new Color(r = 171, g = 0, b = 192),
    new Color(r = 199, g = 0, b = 128),
    new Color(r = 227, g = 0, b = 64)
  )

  val colorsLength = this.colors.length - 1
  val speed = 1 // Speed of color change

  // Gamemode Specific Functions

  def changeColor(node : Cell) {
    if(node.rainbow < 0) {
      node.rainbow = Math.floor(Math.random() * this.colors.length).toInt
    }

    if(node.rainbow >= this.colorsLength) {
      node.rainbow = 0
    }

    node.color = this.colors(node.rainbow)
    node.rainbow += this.speed
  }

  // Override

  override def onTick(gameServer : GameServer) {
    // Change color
    gameServer.nodes.filter(null != _).foreach(this.changeColor)
  }

  override def createFood(gameServer : GameServer, position : Position) = {
    new RainbowFood(gameServer.getNextNodeId, None, position, gameServer.config.foodMass, gameServer)
  }
}