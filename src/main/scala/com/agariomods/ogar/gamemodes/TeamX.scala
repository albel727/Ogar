package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.{Cell, ModeWithMotherCells, ModeWithPushableViruses, PlayerCell}
import com.agariomods.ogar.{Color, GameServer, PlayerTracker}

import scala.util.Random

class TeamX extends Teams with ModeWithPushableViruses with ModeWithMotherCells {

  override val ID = 14
  override val name = "Experimental Team"

  // configurations:
  val teamCollision = false
  // set to true to disable eating teammates

  override val colorFuzziness = 72

  // game mode data:
  override val colors = Array(
    new Color(r = 255, g = 7, b = 7),
    new Color(r = 7, g = 255, b = 7),
    new Color(r = 7, g = 7, b = 255)
  )

  // Override

  override def getRandomSpawn(gameServer : GameServer) = gameServer.getRandomPosition

  override def onCellMove(x1 : Int, y1 : Int, cell : PlayerCell): Unit = {
    if(this.teamCollision) {
      super.onCellMove(x1, y1, cell)
    }
  }

  override def getRandomColor(gameServer : GameServer) = {
    val colorRGB = Random.shuffle(Seq(0xFF, 0x07, (Math.random() * 256).floor.toInt))
    new Color(
      r = colorRGB(0),
      b = colorRGB(1),
      g = colorRGB(2)
    )
  }

  override def canEatTeammate(cell : PlayerCell, check : Cell) : Boolean = {
    if (teamCollision || countNotInRange(check.owner.get) == 1) {
      // Don't eat teammates at all, or at least not their last cells
      return false
    }

    canEatPlayer(cell, check)
  }

  def countNotInRange(client : PlayerTracker) = {
    client.cells.count(!_.inRange)
  }

  override def fuzzColorComponent(component : Int) = {
    if(component != 255) {
      (Math.random() * (this.colorFuzziness - 7) + 7).toInt
    } else component
  }

  override def getTeamColor(team : Int) = {
    val color = this.colors(team)
    new Color(
      r = this.fuzzColorComponent(color.r),
      b = this.fuzzColorComponent(color.b),
      g = this.fuzzColorComponent(color.g)
    )
  }
}