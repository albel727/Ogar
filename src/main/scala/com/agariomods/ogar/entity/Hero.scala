package com.agariomods.ogar.entity

import com.agariomods.ogar.gamemodes.Mode
import com.agariomods.ogar.{Color, GameServer, PlayerTracker, Position}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

object Hero {
  val logger = LoggerFactory.getLogger(classOf[Hero])
}

class Hero(
            _nodeId : Int,
            _owner : Option[PlayerTracker], // playerTracker that owns this cell
            _position : Position,
            _mass : Double,
            _gameServer : GameServer
            )
  extends Cell(CellTypes.Hero, _nodeId, _owner, _position, _mass, _gameServer)
  with FeedableCell
{
  //this.spiked = 1
  this.color = new Color(r = 255, g = 255, b = 7)
  this.mass = 60

  override def getName = "HERO"

  override def onAdd(gameServer : GameServer) {
    gameServer.gameMode.asInstanceOf[ModeWithHeroCells].heroes += this
  }

  override def onRemove(gameServer : GameServer) {
    val gameMode = gameServer.gameMode.asInstanceOf[ModeWithHeroCells]
    val index = gameMode.heroes.indexOf(this)
    if(index != -1) {
      gameMode.heroes.remove(index)
    } else {
      Hero.logger.warn("Tried to remove a non existing HERO node!")
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
    val gameMode = gameServer.gameMode.asInstanceOf[ModeWithHeroCells]

    // Called when the cell is consumed
    val client = consumer.owner.get
    consumer.addMass(this.mass) // delicious

    if(gameMode.isCrazy(client)) {
      // Neutralize the Zombie effect
      client.cured = true
    } else {
      // Become a hero
      client.eatenHeroTimer = gameMode.heroEffectDuration
      client.heroColorFactor = 0

      // Merge immediately
      client.cells.foreach(_.recombineTicks = 0)
    }
  }
}

trait PlayerTrackerWithHeroCells {
  this : PlayerTracker =>

  var eatenHeroTimer : Int = 0
  var crazyTimer : Int = 0
  var cured = false

  var heroColorFactor : Int = 0
  var colorToggle : Int = 0

  def hasEatenHero = {
    this.eatenHeroTimer > 0
  }

  def isCrazy = {
    this.crazyTimer > 0
  }
}

trait ModeWithHeroCells extends Mode {

  val crazyDuration = 200 // ticks
  val heroEffectDuration = 1000 // ticks

  def heroes : ArrayBuffer[Hero]

  def boostSpeedCell(cell : PlayerCell)

  def resetSpeedCell(cell : PlayerCell)

  def isZombie(client : PlayerTracker) : Boolean

  def turnToZombie(client : PlayerTracker)

  def hasEatenHero(client : PlayerTracker with PlayerTrackerWithHeroCells) = client.hasEatenHero

  def isCrazy(client : PlayerTracker with PlayerTrackerWithHeroCells) = client.isCrazy && !isZombie(client)

  def tickHeroColor(client : PlayerTracker with PlayerTrackerWithHeroCells) {
    if(this.isCrazy(client)) {
      client.crazyTimer -= 1
      if(client.crazyTimer == 0) {
        // reset speed:
        this.resetSpeed(client)

        if(client.cured) {
          // reset color:
          client.cells.foreach(_.setColor(client.color))
          client.cured = false // reset
        } else {
          // turn player to zombie
          this.turnToZombie(client)
        }
      } else {
        client.colorToggle += 1
        if(client.colorToggle % 10 == 0) {
          var blinkColor : Color = null

          if(client.colorToggle == 20) {
            blinkColor = client.color
            client.colorToggle = 0
          } else {
            if(client.cured) {
              blinkColor = new Color(r = 255, g = 255, b = 7) // Yellow
            } else {
              blinkColor = new Color(r = 75, g = 75, b = 75) // Gray
            }
          }

          client.cells.foreach(_.setColor(blinkColor))
        }
      }
    } else if(this.hasEatenHero(client)) {
      client.eatenHeroTimer -= 1
      var color : Color = null
      if(client.eatenHeroTimer > 0) {
        client.heroColorFactor = (client.heroColorFactor + 5) % 401
        if(client.heroColorFactor <= 200) {
          color = new Color(r = 255, g = 255, b = client.heroColorFactor) // Yellow scheme
        } else {
          color = new Color(r = 255, g = 255, b = 400 - client.heroColorFactor) // Yellow scheme
        }
      } else {
        color = client.color // reset
      }

      client.cells.foreach(_.setColor(color))
    }
  }

  override def canEatOther(cell : PlayerCell, check : Cell) : Boolean = {
    check.getType match {
      case CellTypes.Hero =>
        // Zombies can't eat Hero cells
        if(isZombie(cell.owner.get)) {
          return false
        }
      case _ =>
    }

    super.canEatOther(cell, check)
  }

  override def createPlayer(client : PlayerTracker, position : Position, mass : Double) = {
    val result = super.createPlayer(client, position, mass)

    if (isCrazy(client)) {
      boostSpeedCell(result)
    }

    result
  }

  private def resetSpeed(client : PlayerTracker) {
    client.cells.foreach(this.resetSpeedCell)
  }

  abstract override def calcMergeTime(cell : HasMergeTime, base : Int) = {
    if(cell.owner.exists(hasEatenHero)) {
      cell.recombineTicks = 0
    } else {
      super.calcMergeTime(cell, base)
    }
  }
}