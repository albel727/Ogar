package com.agariomods.ogar.entity

import com.agariomods.ogar.{GameServer, PlayerTracker, Position}
import org.slf4j.LoggerFactory

object Virus {
  val logger = LoggerFactory.getLogger(classOf[Virus])
}

class Virus(
             _nodeId : Int,
             _owner : Option[PlayerTracker], // playerTracker that owns this cell
             _position : Position,
             _mass : Double,
             _gameServer : GameServer
             )
  extends Cell(CellTypes.Virus, _nodeId, _owner, _position, _mass, _gameServer)
  with ExploderCell
  with FeedableCell
{
  this.spiked = 1

  var fed = 0

  def feed(feeder : Cell, gameServer : GameServer) {
    this.setAngle(feeder.getAngle) // Set direction if the virus explodes
    this.mass += feeder.mass
    this.fed += 1 // Increase feed count
    gameServer.removeNode(feeder)

    // Check if the virus is going to explode
    if (this.fed >= gameServer.config.virusFeedAmount) {
      this.mass = gameServer.config.virusStartMass // Reset mass
      this.fed = 0
      gameServer.shootVirus(this)
    }

  }

  // Main Functions

  override def getEatingRange = {
    this.getSize * 0.4 // 0 for ejected cells
  }

  override def onAdd(gameServer : GameServer) {
    gameServer.nodesVirus.add(this)
  }

  override def onRemove(gameServer : GameServer) {
    if(!gameServer.nodesVirus.remove(this)) {
      Virus.logger.warn("Tried to remove a non existing virus!")
    }
  }
}
