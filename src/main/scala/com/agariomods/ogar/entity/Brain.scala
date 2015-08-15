package com.agariomods.ogar.entity

import com.agariomods.ogar.gamemodes.Mode
import com.agariomods.ogar.{Color, GameServer, PlayerTracker, Position}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

object Brain {
  val logger = LoggerFactory.getLogger(classOf[Brain])
}

class Brain(
             _nodeId : Int,
             _owner : Option[PlayerTracker], // playerTracker that owns this cell
             _position : Position,
             _mass : Double,
             _gameServer : GameServer
             )
  extends Cell(CellTypes.Brain, _nodeId, _owner, _position, _mass, _gameServer)
  with FeedableCell
{
  //this.spiked = 1
  this.color = new Color(r = 255, g = 7, b = 255)
  this.mass = 60

  override def getName = "BRAIN"

  override def onAdd(gameServer : GameServer) {
    gameServer.gameMode.asInstanceOf[ModeWithBrainCells].brains += this
  }

  override def onRemove(gameServer : GameServer) {
    val gameMode = gameServer.gameMode.asInstanceOf[ModeWithBrainCells]
    val index = gameMode.brains.indexOf(this)
    if(index != -1) {
      gameMode.brains.remove(index)
    } else {
      Brain.logger.warn("Tried to remove a non existing BRAIN node!")
    }
  }

  def feed(feeder : Cell, gameServer : GameServer) {
    gameServer.removeNode(feeder)

    this.setAngle(feeder.getAngle)
    this.moveEngineTicks = 5 // Amount of times to loop the movement function
    this.moveEngineSpeed = 60

    if(!gameServer.movingNodes.contains(this)) {
      gameServer.movingNodes += this
    }
  }

  override def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    val gameMode = gameServer.gameMode.asInstanceOf[ModeWithBrainCells]

    // Called when the cell is consumed
    val client = consumer.owner.get
    consumer.addMass(this.mass) // yummy!

    client.eatenBrainTimer = gameMode.brainEffectDuration

    // Boost speed
    client.cells.foreach(gameMode.boostSpeedCell)
  }
}

trait PlayerTrackerWithBrainCells {
  this : PlayerTracker =>

  var eatenBrainTimer : Int = 0

  var zColorFactor : Int = 0
  var zColorIncr : Boolean = false // color will be increased if TRUE - otherwise it will be decreased.

  def hasEatenBrain = {
    this.eatenBrainTimer > 0
  }
}

trait ModeWithBrainCells extends Mode {

  val brainEffectDuration = 200 // ticks

  val colorFactorStep = 5
  val colorLower = 50 // Min 0
  val colorUpper = 225 // Max 255

  def brains : ArrayBuffer[Brain]

  def boostSpeedCell(cell : PlayerCell)

  def resetSpeedCell(cell : PlayerCell)

  def isZombie(client : PlayerTracker) : Boolean

  def createZColorFactor(client : PlayerTracker with PlayerTrackerWithBrainCells) {
    client.zColorFactor = (Math.random() * (this.colorUpper - this.colorLower + 1)).floor.toInt + this.colorLower
    client.zColorIncr = true // color will be increased if TRUE - otherwise it will be decreased.
  }

  def nextZColorFactor(client : PlayerTracker with PlayerTrackerWithBrainCells) {
    if(client.zColorIncr) {
      if(client.zColorFactor + this.colorFactorStep >= this.colorUpper) {
        client.zColorFactor = this.colorUpper
        client.zColorIncr = false
      }
      else {
        client.zColorFactor += this.colorFactorStep
      }
    }
    else {
      if(client.zColorFactor - this.colorFactorStep <= this.colorLower) {
        client.zColorFactor = this.colorLower
        client.zColorIncr = true
      }
      else {
        client.zColorFactor -= this.colorFactorStep
      }
    }
  }

  def updateZColor(client : PlayerTracker with PlayerTrackerWithBrainCells, mask : Int) {
    val color = new Color(
      r = if ((mask & 0x4) > 0) client.zColorFactor else 7,
      g = if ((mask & 0x2) > 0) client.zColorFactor else 7,
      b = if ((mask & 0x1) > 0) client.zColorFactor else 7
    )
    client.color = new Color(color)

    client.cells.foreach(_.setColor(color))
  }

  def hasEatenBrain(client : PlayerTracker with PlayerTrackerWithBrainCells) = client.hasEatenBrain

  def tickZombieColor(client : PlayerTracker with PlayerTrackerWithBrainCells) {
    this.nextZColorFactor(client)

    if(this.hasEatenBrain(client)) {
      client.eatenBrainTimer -= 1

      if(client.eatenBrainTimer > 0) {
        this.updateZColor(client, 0x5) // Pink
        return // continue
      } else {
        // reset speed:
        this.resetSpeed(client)
      }
    }

    this.updateZColor(client, 0x7) // Gray
  }

  override def canEatOther(cell : PlayerCell, check : Cell) : Boolean = {
    check.getType match {
      case CellTypes.Brain =>
        // Non-zombies can't eat Brain cells
        if(!isZombie(cell.owner.get)) {
          return false
        }
      case _ =>
    }

    super.canEatOther(cell, check)
  }

  override def createPlayer(client : PlayerTracker, position : Position, mass : Double) = {
    val result = super.createPlayer(client, position, mass)

    if (hasEatenBrain(client)) {
      boostSpeedCell(result)
    }

    result
  }

  private def resetSpeed(client : PlayerTracker) {
    client.cells.foreach(this.resetSpeedCell)
  }
}