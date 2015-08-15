package com.agariomods.ogar.entity

import com.agariomods.ogar._
import com.agariomods.ogar.gamemodes.Mode

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks


trait ModeWithMotherCells extends Mode {
  this : Mode =>

  // Gamemode Specific Variables
  val nodesMother = new ArrayBuffer[MotherCell]()
  var tickMother = 0
  var tickMotherS = 0

  // Config
  val motherCellMass = 200d
  val motherUpdateInterval = 5 // How many ticks it takes to update the mother cell (1 tick = 50 ms)
  val motherSpawnInterval = 100 // How many ticks it takes to spawn another mother cell - Currently 5 seconds
  val motherMinAmount = 5

  def updateMotherCells(gameServer : GameServer) {
    this.nodesMother.foreach(mother => {
      // Checks
      mother.update(gameServer)
      mother.checkEat(gameServer)
    })
  }

  def spawnMotherCell(gameServer : GameServer) {
    // Checks if there are enough mother cells on the map
    if(this.nodesMother.length < this.motherMinAmount) {
      // Spawns a mother cell
      val pos = gameServer.getRandomPosition

      // Check for players
      gameServer.nodesPlayer.foreach(check => Breaks.breakable {
        val r = check.getSize // Radius of checking player cell

        // Collision box
        val topY = check.position.y - r
        val bottomY = check.position.y + r
        val leftX = check.position.x - r
        val rightX = check.position.x + r

        // Check for collisions
        if(pos.y > bottomY) {
          Breaks.break() // continue
        }

        if(pos.y < topY) {
          Breaks.break() // continue
        }

        if(pos.x > rightX) {
          Breaks.break() // continue
        }

        if(pos.x < leftX) {
          Breaks.break() // continue
        }

        // Collided
        return
      })

      // Spawn if no cells are colliding
      val m = new MotherCell(gameServer.getNextNodeId, None, pos, this.motherCellMass, gameServer)
      gameServer.addNode(m)
    }
  }

  override def onTick(gameServer : GameServer) {
    // Mother Cell updates
    if(this.tickMother >= this.motherUpdateInterval) {
      this.updateMotherCells(gameServer)
      this.tickMother = 0
    } else {
      this.tickMother += 1
    }

    // Mother Cell Spawning
    if(this.tickMotherS >= this.motherSpawnInterval) {
      this.spawnMotherCell(gameServer)
      this.tickMotherS = 0
    } else {
      this.tickMotherS += 1
    }

    super.onTick(gameServer)
  }

  override def onChange(gameServer : GameServer) {
    // Remove all mother cells
    while(this.nodesMother.nonEmpty) {
      gameServer.removeNode(this.nodesMother.head)
    }

    super.onChange(gameServer)
  }
}

class MotherCell(
                  _nodeId : Int,
                  _owner : Option[PlayerTracker], // playerTracker that owns this cell
                  _position : Position,
                  _mass : Double,
                  _gameServer : GameServer
                  )
  extends Cell(CellTypes.Virus, _nodeId, _owner, _position, _mass, _gameServer) // Copies virus cell
  with ExploderCell {

  this.color = new Color(r = 205, g = 85, b = 100)
  this.spiked = 1

  override def getEatingRange = {
    this.getSize * 0.5
  }

  def update(gameServer : GameServer) {
    // Add mass
    this.mass += 0.25

    // Spawn food
    val maxFood = 10
    // Max food spawned per tick
    var i = 0 // Food spawn counter
    while ((this.mass > gameServer.gameMode.asInstanceOf[ModeWithMotherCells].motherCellMass) && (i < maxFood)) {
      // Only spawn if food cap hasn been reached
      if(gameServer.currentFood < gameServer.config.foodMaxAmount) {
        this.spawnFood(gameServer)
      }

      // Incrementers
      this.mass -= 1
      i += 1
    }
  }

  def checkEat(gameServer : GameServer) {
    val safeMass = this.mass * 0.9
    val r = this.getSize // The box area that the checked cell needs to be in to be considered eaten

    // Loop for potential prey
    gameServer.nodesPlayer.foreach(check => Breaks.breakable {
      if(check.mass > safeMass) {
        // Too big to be consumed
        Breaks.break() // continue
      }

      // Calculations
      val len = r - (check.getSize / 2.0).floor.toInt
      if((this.abs(this.position.x - check.position.x) < len) && (this.abs(this.position.y - check.position.y) < len)) {
        // A second, more precise check
        val xs = Math.pow(check.position.x - this.position.x, 2)
        val ys = Math.pow(check.position.y - this.position.y, 2)
        val dist = Math.sqrt(xs + ys)

        if(r > dist) {
          // Eats the cell
          gameServer.removeNode(check)
          this.mass += check.mass
        }
      }
    })
    gameServer.movingNodes.foreach(check => Breaks.breakable {
      if((check.getType == CellTypes.Food) || (check.mass > safeMass)) {
        // Too big to be consumed/ No player cells
        Breaks.break() // continue
      }

      // Calculations
      val len = r.floor.toInt
      if((this.abs(this.position.x - check.position.x) < len) && (this.abs(this.position.y - check.position.y) < len)) {
        // Eat the cell
        gameServer.removeNode(check)
        this.mass += check.mass
      }
    })
  }

  final def abs(n : Int) = {
    // Because Math.abs is slow
    if (n < 0) -n else n
  }

  def spawnFood(gameServer : GameServer) {
    // Get starting position
    val angle = Math.random() * 6.28
    // (Math.PI * 2) ??? Precision is not our greatest concern here
    val r = this.getSize
    val pos = Position.toInt(
      x = this.position.x + r * Math.sin(angle),
      y = this.position.y + r * Math.cos(angle)
    )

    // Spawn food
    val f = gameServer.gameMode.createFood(gameServer, pos)
    f.setColor(gameServer.getRandomColor)

    gameServer.addNode(f)

    // Move engine
    f.angle = angle
    val dist = (Math.random() * 10) + 22 // Random distance
    f.setMoveEngineData(dist, 15)

    gameServer.setAsMovingNode(f)
  }

  override def onAdd(gameServer : GameServer) {
    gameServer.gameMode.asInstanceOf[ModeWithMotherCells].nodesMother += this // Temporary
  }

  override def onRemove(gameServer : GameServer) {
    gameServer.gameMode.asInstanceOf[ModeWithMotherCells].nodesMother -= this
  }

  override def visibleCheck(box : Box, centerPos : Position) = {
    // Checks if this cell is visible to the player
    val cellSize = this.getSize
    val lenX = cellSize + box.width.floor.toInt
    // Width of cell + width of the box (Int)
    val lenY = cellSize + box.height.floor.toInt // Height of cell + height of the box (Int)

    (this.abs(this.position.x - centerPos.x) < lenX) && (this.abs(this.position.y - centerPos.y) < lenY)
  }
}
