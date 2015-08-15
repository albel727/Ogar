package com.agariomods.ogar.entity

import com.agariomods.ogar._
import com.agariomods.ogar.gamemodes.RainbowCell

class Cell(
            val cellType : CellType,
            val nodeId : Int,
            val owner : Option[PlayerTracker], // playerTracker that owns this cell
            var position : Position,
            var mass : Double,
            val gameServer : GameServer
            ) extends RainbowCell {

  var spiked : Byte = 0 // If 1, then this cell has spikes around it

  var killedBy : Option[Cell] = None // Cell that ate this cell

  var moveEngineTicks = 0 // Amount of times to loop the movement function
  var moveEngineSpeed = 0d
  var moveDecay = .75
  var angle = 0d // Angle of movement

  var color : Color = new Color(255, 255, 255)

  var inRange = false

  def getName = owner.map(_.name).getOrElse("")

  def setColor(color : Color): Unit = {
    this.color = color
  }

  def getColor = color

  def getType = cellType

  def getSize = {
    // Calculates radius based on cell mass
    Math.ceil(Math.sqrt(100 * this.mass)).toInt
  }

  def getSquareSize = {
    // R * R
    (100 * this.mass).floor.toInt
  }

  def addMass(n : Double) {
    this.owner.foreach(owner => {
      val server = owner.gameServer
      if(this.mass + n > server.config.playerMaxMass && owner.cells.size < server.config.playerMaxCells) {
        this.mass = (this.mass + n) / 2.0
        server.newCellVirused(owner, this.asInstanceOf[PlayerCell], 0, this.mass, 150)
      } else {
        this.mass = Math.min(this.mass + n,server.config.playerMaxMass)
      }
    })
  }

  def getSpeed = {
    // Old formula: 5 + (20 * (1 - (this.mass/(70+this.mass))));
    // Based on 50ms ticks. If updateMoveEngine interval changes, change 50 to new value
    // (should possibly have a config value for this?)
    30 * Math.pow(this.mass, -1.0 / 4.5) * 50 / 40.0
  }

  def setAngle(radians : Double) = {
    this.angle = radians
  }

  def getAngle = angle

  def setMoveEngineData(speed : Double, ticks : Int, decay : Double = Double.NaN) {
    this.moveEngineSpeed = speed
    this.moveEngineTicks = ticks
    this.moveDecay = if(decay.isNaN) 0.75 else decay
  }

  def getEatingRange = {
    0d // 0 for ejected cells
  }

  def getKiller = killedBy

  def setKiller(cell : Cell) {
    this.killedBy = Some(cell)
  }

  // Functions

  def collisionCheck(bottomY : Int, topY : Int, rightX : Int, leftX : Int) : Boolean = {
    // Collision checking
    if (this.position.y > bottomY) return false

    if (this.position.y < topY) return false

    if (this.position.x > rightX) return false

    if (this.position.x < leftX) return false

    true
  }

  // This collision checking function is based on CIRCLE shape
  def collisionCheck2(objectSquareSize : Int, objectPosition : Position) = {
    // IF (O1O2 + r <= R) THEN collided. (O1O2: distance b/w 2 centers of cells)
    // (O1O2 + r)^2 <= R^2
    // approximately, remove 2*O1O2*r because it requires sqrt(): O1O2^2 + r^2 <= R^2

    val dx = this.position.x - objectPosition.x
    val dy = this.position.y - objectPosition.y

    dx * dx + dy * dy + this.getSquareSize <= objectSquareSize
  }

  def visibleCheck(box : Box, centerPos : Position) = {
    // Checks if this cell is visible to the player
    this.collisionCheck(box.bottomY, box.topY, box.rightX, box.leftX)
  }

  def calcMovePhys(config : GameConfig) {
    // Movement for ejected cells
    var x = this.position.x + this.moveEngineSpeed * Math.sin(this.angle)
    var y = this.position.y + this.moveEngineSpeed * Math.cos(this.angle)

    // Movement engine
    this.moveEngineSpeed *= this.moveDecay // Decaying speed
    this.moveEngineTicks -= 1

    // Border check - Bouncy physics
    val radius = 40
    if ((this.position.x - radius) < config.borderLeft) {
      // Flip angle horizontally - Left side
      this.angle = 6.28 - this.angle
      x = config.borderLeft + radius
    }
    if ((this.position.x + radius) > config.borderRight) {
      // Flip angle horizontally - Right side
      this.angle = 6.28 - this.angle
      x = config.borderRight - radius
    }
    if ((this.position.y - radius) < config.borderTop) {
      // Flip angle vertically - Top side
      this.angle = if (this.angle <= 3.14) {
        3.14 - this.angle
      } else {
        9.42 - this.angle
      }
      y = config.borderTop + radius
    }
    if ((this.position.y + radius) > config.borderBottom) {
      // Flip angle vertically - Bottom side
      this.angle = if (this.angle <= 3.14) {
        3.14 - this.angle
      } else {
        9.42 - this.angle
      }
      y = config.borderBottom - radius
    }

    // Set position
    this.position = Position.toInt(x = x, y = y)
  }

  // Override these

  def sendUpdate() = {
    // Whether or not to include this cell in the update packet
    true
  }

  def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    // Called when the cell is consumed
  }

  def onAdd(gameServer : GameServer) {
    // Called when this cell is added to the world
  }

  def onRemove(gameServer : GameServer) {
    // Called when this cell is removed
  }

  def onAutoMove(gameServer : GameServer) : Boolean = {
    // Called on each auto move engine tick
    false
  }

  def moveDone(gameServer : GameServer) {
    // Called when this cell finished moving with the auto move engine
  }
}

