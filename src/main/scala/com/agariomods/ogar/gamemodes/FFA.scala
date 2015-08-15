package com.agariomods.ogar.gamemodes

import com.agariomods.ogar._

class FFA extends Mode {
  override val ID = 0
  override val name = "Free For All"
  override val specByLeaderboard = true

  // Override

  override def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // Random color
    player.color = gameServer.getRandomColor

    // Check if there are ejected mass in the world.
    if(gameServer.nodesEjected.size > 0) {
      val index = Math.floor(Math.random() * 100) + 1
      if(index <= gameServer.config.ejectSpawnPlayer) {
        // Get ejected cell
        val index = Math.floor(Math.random() * gameServer.nodesEjected.size).toInt
        val e = gameServer.nodesEjected.drop(index).head

        // Remove ejected mass
        gameServer.removeNode(e)

        // Inherit
        val pos = e.position
        val startMass = e.mass

        val color = e.getColor
        player.setColor(color)

        // Spawn player
        gameServer.spawnPlayer(player, pos, startMass)
        return
      }
    }

    // Spawn player
    gameServer.spawnPlayer(player)
  }

  override def updateLB(gameServer : GameServer) {
    val lb = gameServer.leaderboard.asInstanceOf[PlayerLeaderboard].leaders

    import com.agariomods.ogar.utils.IterExt._

    // Loop through all clients
    gameServer.clients.filter(null != _).map(_.playerTracker).filter(_.cells.size > 0).getTop(10, (recalc : Boolean) => (pl : PlayerTracker) => pl.getScore(recalc), lb)

    this.rankOne = lb.headOption.orNull
  }
}