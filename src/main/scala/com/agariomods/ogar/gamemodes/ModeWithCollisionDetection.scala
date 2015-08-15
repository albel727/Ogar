package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.{CellTypes, PlayerCell}
import com.agariomods.ogar.{PlayerTracker, Position}

import scala.util.control.Breaks

trait ModeWithCollisionDetection extends Mode {
  override def onCellMove(x1 : Int, y1 : Int, cell : PlayerCell) {
    super.onCellMove(x1, y1, cell)

    val cellOwner = cell.owner.get
    val r = cell.getSize

    cellOwner.visibleNodes.foreach(check => Breaks.breakable {
      // Only collide with player cells
      if (check.getType != CellTypes.Player) {
        Breaks.break() // continue
      }

      val checkOwner = check.owner.get

      // Don't handle collision with own cells
      if (cellOwner == checkOwner) {
        Breaks.break() // continue
      }

      if(canCollide(cellOwner, checkOwner)) {
        // Check if in collision range
        val collisionDist = check.getSize + r // Minimum distance between the 2 cells
        if(!cell.simpleCollide(check.position, collisionDist)) {
          // Skip
          Breaks.break() // continue
        }

        // First collision check passed... now more precise checking
        val dist = cell.getDist(cell.position.x, cell.position.y, check.position.x, check.position.y)

        // Calculations
        if(dist < collisionDist) {
          // Collided
          onCollision(cellOwner, checkOwner)

          // The moving cell pushes the colliding cell
          val newDeltaY = check.position.y - y1
          val newDeltaX = check.position.x - x1
          val newAngle = Math.atan2(newDeltaX, newDeltaY)

          val move = collisionDist - dist

          check.position = Position.toInt(
            x = check.position.x + move * Math.sin(newAngle),
            y = check.position.y + move * Math.cos(newAngle)
          )
        }
      }
    })
  }

  def canCollide(cellOwner : PlayerTracker, checkOwner : PlayerTracker) : Boolean = {
    false
  }

  def onCollision(cellOwner : PlayerTracker, checkOwner : PlayerTracker): Unit = {
  }
}
