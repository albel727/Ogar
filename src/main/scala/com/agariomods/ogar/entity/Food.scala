package com.agariomods.ogar.entity

import com.agariomods.ogar.{GameServer, PlayerTracker, Position}

class Food(
            _nodeId : Int,
            _owner : Option[PlayerTracker], // playerTracker that owns this cell
            _position : Position,
            _mass : Double,
            _gameServer : GameServer
            ) extends Cell(CellTypes.Food, _nodeId, _owner, _position, _mass, _gameServer) {

  override val getSize = Math.ceil(Math.sqrt(100 * this.mass)).toInt
  override val getSquareSize = (100 * this.mass).floor.toInt // not being decayed -> calculate one time

  // Main Functions

  override def sendUpdate() = {
    // Whether or not to include this cell in the update packet
    this.moveEngineTicks != 0
  }

  override def onAdd(gameServer : GameServer): Unit = {
    gameServer.currentFood += 1
  }

  override def onRemove(gameServer : GameServer) {
    gameServer.currentFood -= 1
  }

  override def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    consumer.addMass(this.mass)
  }
}
