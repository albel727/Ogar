package com.agariomods.ogar.entity

import com.agariomods.ogar.{Box, GameServer, PlayerTracker, Position}

import scala.util.control.Breaks._

trait HasMergeTime extends Cell {
  var recombineTicks = 0 // Ticks until the cell can recombine with other cells
}

class PlayerCell(
                  _nodeId : Int,
                  _owner : Option[PlayerTracker], // playerTracker that owns this cell
                  _position : Position,
                  _mass : Double,
                  _gameServer : GameServer
                  )
  extends Cell(CellTypes.Player, _nodeId, _owner, _position, _mass, _gameServer)
  with SpeedableCell
  with HasMergeTime
{

  var ignoreCollision = false // This is used by player cells so that they dont cause any problems when splitting

  // Main Functions

  override def visibleCheck(box : Box, centerPos : Position) : Boolean = {
    // Use old fashioned checking method if cell is small
    if (this.mass < 150) {
      return this.collisionCheck(box.bottomY,box.topY,box.rightX,box.leftX)
    }

    // Checks if this cell is visible to the player
    val cellSize = this.getSize
    val lenX = (cellSize + box.width).floor.toInt // Width of cell + width of the box (Int)
    val lenY = (cellSize + box.height).floor.toInt // Height of cell + height of the box (Int)

    (Math.abs(this.position.x - centerPos.x) < lenX) && (Math.abs(this.position.y - centerPos.y) < lenY)
  }

  def simpleCollide(check : Position, d : Double) = {
    // Simple collision check
    val len = (2 * d).floor.toInt // Width of cell + width of the box (Int)

    (Math.abs(this.position.x - check.x) < len) &&
      (Math.abs(this.position.y - check.y) < len)
  }

  // Movement

  def calcMove(mousePos : Position, gameServer : GameServer) {
    val owner = this.owner.get

    val config = gameServer.config
    val r = this.getSize // Cell radius

    // Get angle
    val deltaX = mousePos.x - this.position.x
    val deltaY = mousePos.y - this.position.y
    val angle = Math.atan2(deltaX, deltaY)
    if(angle.isNaN) {
      return
    }

    // Distance between mouse pointer and cell
    var dist = this.getDist(this.position.x, this.position.y, mousePos.x, mousePos.y)
    val speed = Math.min(this.getSpeed, dist)

    var x1 = this.position.x + speed * Math.sin(angle)
    var y1 = this.position.y + speed * Math.cos(angle)

    // Check to ensure we're not passing the world border
    if (x1 < config.borderLeft) {
      x1 = config.borderLeft
    }
    if (x1 > config.borderRight) {
      x1 = config.borderRight
    }
    if (y1 < config.borderTop) {
      y1 = config.borderTop
    }
    if (y1 > config.borderBottom) {
      y1 = config.borderBottom
    }

    val newPos = Position.toInt(x = x1, y = y1)

    // Collision check for other cells
    owner.cells.foreach(cell => {
      breakable {
        if((this.nodeId == cell.nodeId) || this.ignoreCollision) {
          break() //continue
        }

        if((cell.recombineTicks > 0) || (this.recombineTicks > 0)) {
          // Cannot recombine - Collision with your own cells
          val collisionDist = cell.getSize + r // Minimum distance between the 2 cells
          if(!this.simpleCollide(cell.position, collisionDist)) {
            // Skip
            break() //continue
          }

          // First collision check passed... now more precise checking
          dist = this.getDist(this.position.x, this.position.y, cell.position.x, cell.position.y)

          // Calculations
          if(dist < collisionDist) {
            // Collided
            // The moving cell pushes the colliding cell
            val newDeltaY = cell.position.y - y1
            val newDeltaX = cell.position.x - x1
            val newAngle = Math.atan2(newDeltaX, newDeltaY)

            val move = collisionDist - dist + 5

            cell.position = Position.toInt(
              x = cell.position.x + move * Math.sin(newAngle),
              y = cell.position.y + move * Math.cos(newAngle)
            )
          }
        }
      }
    })

    gameServer.gameMode.onCellMove(x1.toInt, y1.toInt, this)

    this.position = newPos
  }

  // Override

  override def getEatingRange = {
    this.getSize * 0.4
  }

  override def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    consumer.addMass(this.mass)
  }

  override def onAdd(gameServer : GameServer) {
    // Add to special player node list
    gameServer.nodesPlayer.add(this)
    // Gamemode actions
    gameServer.gameMode.onCellAdd(this)
  }

  override def onRemove(gameServer : GameServer) {
    val owner = this.owner.get

    // Remove from player cell list
    owner.cells.remove(this)
    // Remove from special player controlled node list
    gameServer.nodesPlayer.remove(this)
    // Gamemode actions
    gameServer.gameMode.onCellRemove(this)

    owner.mouseCells.remove(this.nodeId)
  }

  override def moveDone(gameServer : GameServer) {
    this.ignoreCollision = false
  }

  // Lib

  final def getDist(x1 : Double, y1 : Double, x2 : Double, y2 : Double) = {
    var xs = x2 - x1
    xs = xs * xs

    var ys = y2 - y1
    ys = ys * ys

    Math.sqrt(xs + ys)
  }
}
