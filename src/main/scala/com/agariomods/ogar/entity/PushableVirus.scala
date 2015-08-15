package com.agariomods.ogar.entity

import com.agariomods.ogar.gamemodes.Mode
import com.agariomods.ogar.{GameServer, PlayerTracker, Position}

trait ModeWithPushableViruses extends Mode {
  this : Mode =>

  var pushVirus = true // true: pushing virus, false: splitting virus

  override def createVirus(gameServer : GameServer, position : Position) = {
    if(pushVirus) {
      // Special virus mechanics
      new PushableVirus(gameServer.getNextNodeId, None, position, gameServer.config.virusStartMass, gameServer)
    } else {
      super.createVirus(gameServer, position)
    }
  }
}

class PushableVirus(
                     _nodeId : Int,
                     _owner : Option[PlayerTracker], // playerTracker that owns this cell
                     _position : Position,
                     _mass : Double,
                     _gameServer : GameServer
                     ) extends Virus(_nodeId, _owner, _position, _mass, _gameServer) {

  override def feed(feeder : Cell, gameServer : GameServer) {
    gameServer.removeNode(feeder)
    // Pushes the virus
    this.setAngle(feeder.getAngle) // Set direction if the virus explodes
    this.moveEngineTicks = 5 // Amount of times to loop the movement function
    this.moveEngineSpeed = 30

    if(!gameServer.movingNodes.contains(this)) {
      gameServer.movingNodes += this
    }
  }
}
