package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.PlayerCell
import com.agariomods.ogar.{Color, GameServer, PlayerLeaderboard, PlayerTracker}

import scala.collection.mutable.ArrayBuffer

class Zombie extends Mode with ModeWithCollisionDetection {

  override val ID = 12
  override val name = "Zombie FFA"
  override val haveTeams = true
  val zombieColor = new Color(r = 223, g = 223, b = 223)
  val zombies = new ArrayBuffer[PlayerCell]
  val players = new ArrayBuffer[PlayerCell]

  // Gamemode Specific Functions

  def makeZombie(player : PlayerTracker) {
    // turns a player into a zombie
    player.team = 0
    player.color = this.zombieColor
    player.cells.foreach(playerCell => {
      // remove cell from players array
      this.players -= playerCell
      // change color of cell
      playerCell.color = this.zombieColor
      // add cell to zombie array
      this.zombies += playerCell
    })
  }

  // Override

  override def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // make player a zombie if there are none
    if(this.zombies.length == 0) {
      player.team = 0
      player.color = this.zombieColor
    } else {
      // use player id as team so that bots are still able to fight (even though they probably turn into zombies very fast)
      player.team = player.pID
      player.color = gameServer.getRandomColor
    }

    // Spawn player
    gameServer.spawnPlayer(player)
  }

  override def onCellAdd(cell : PlayerCell) {
    // Add to team list
    if(cell.owner.get.getTeam == 0) {
      this.zombies += cell
    } else {
      this.players += cell
    }
  }

  override def onCellRemove(cell : PlayerCell) {
    // Remove from team list
    if(cell.owner.get.getTeam == 0) {
      this.zombies -= cell
    } else {
      this.players -= cell
    }
  }

  override def canCollide(cellOwner : PlayerTracker, checkOwner : PlayerTracker) : Boolean = {
    // Collision with zombies
    checkOwner.getTeam == cellOwner.getTeam || checkOwner.getTeam == 0 || cellOwner.getTeam == 0
  }

  override def onCollision(cellOwner : PlayerTracker, checkOwner : PlayerTracker): Unit = {
    if(checkOwner.getTeam == 0 && cellOwner.getTeam != 0) {
      // turn player into zombie
      this.makeZombie(cellOwner)
    } else if(cellOwner.getTeam == 0 && checkOwner.getTeam != 0) {
      // turn other player into zombie
      this.makeZombie(checkOwner)
    }
  }

  override def updateLB(gameServer : GameServer) {
    val lb = gameServer.leaderboard.asInstanceOf[PlayerLeaderboard].leaders

    import com.agariomods.ogar.utils.IterExt._

    // Loop through all clients
    gameServer.clients.filter(null != _).map(_.playerTracker).filter(_.getTeam != 0).filter(_.cells.size > 0).getTop(10, (recalc : Boolean) => (pl : PlayerTracker) => pl.getScore(recalc), lb)

    this.rankOne = lb.headOption.orNull
  }
}