package com.agariomods.ogar.entity

import com.agariomods.ogar.{GameServer, PlayerTracker, Position}

class EjectedMass(
                   _nodeId : Int,
                   _owner : Option[PlayerTracker], // playerTracker that owns this cell
                   _position : Position,
                   _mass : Double,
                   _gameServer : GameServer
                   ) extends Cell(CellTypes.EjectedMass, _nodeId, _owner, _position, _mass, _gameServer) {

  override val getSize = Math.ceil(Math.sqrt(100 * this.mass)).toInt

  override val getSquareSize = (100 * this.mass).floor.toInt // not being decayed -> calculate one time

  // Main Functions

  override def sendUpdate() = {
    // Whether or not to include this cell in the update packet
    this.moveEngineTicks != 0
  }

  override def onRemove(gameServer : GameServer) {
    // Remove from list of ejected mass
    gameServer.nodesEjected.remove(this)
  }

  override def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    // Adds mass to consumer
    consumer.addMass(this.mass)
  }

  override def onAutoMove(gameServer : GameServer) : Boolean = {
    if (gameServer.nodesVirus.size < gameServer.config.virusMaxAmount) {
      // Check for viruses
      val v = gameServer.getNearestVirus(this)
      if (null != v) { // Feeds the virus if it exists
        v.feed(this,gameServer)
        return true
      }
    }
    false
  }

  override def moveDone(gameServer : GameServer) {
    if (!this.onAutoMove(gameServer)) {
      gameServer.nodesEjected.add(this)
    }
  }
}
