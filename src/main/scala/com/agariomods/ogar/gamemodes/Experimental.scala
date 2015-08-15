package com.agariomods.ogar.gamemodes

import com.agariomods.ogar._
import com.agariomods.ogar.entity._

class Experimental extends FFA with ModeWithPushableViruses with ModeWithMotherCells {
  override val ID = 2
  override val name = "Experimental"
  override val specByLeaderboard = true

  // Override

  override def getRandomSpawn(gameServer : GameServer) = gameServer.getRandomPosition
}
