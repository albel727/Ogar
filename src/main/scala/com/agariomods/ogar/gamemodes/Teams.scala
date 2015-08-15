package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.PlayerCell
import com.agariomods.ogar.{Color, GameServer, PlayerTracker, TeamLeaderboard}

import scala.collection.mutable.ArrayBuffer

class Teams extends Mode with ModeWithCollisionDetection {

  override val ID = 1
  override val name = "Teams"
  override val decayMod = 1.5
  override val haveTeams = true
  val colorFuzziness = 32

  // Special
  var teamAmount = 3
  // Amount of teams. Having more than 3 teams will cause the leaderboard to work incorrectly (client issue).
  val colors = Array(
    new Color(r = 223, g = 0, b = 0),
    new Color(r = 0, g = 223, b = 0),
    new Color(r = 0, g = 0, b = 223)
  ) // Make sure you add extra colors here if you wish to increase the team amount [Default colors are: Red, Green, Blue]

  var nodes = new Array[ArrayBuffer[PlayerCell]](teamAmount)

  // Teams

  //Gamemode Specific Functions

  def fuzzColorComponent(component : Int) = {
    component + (Math.random() * this.colorFuzziness).floor.toInt
  }

  def getTeamColor(team : Int) = {
    val color = this.colors(team)
    new Color(
      r = this.fuzzColorComponent(color.r),
      b = this.fuzzColorComponent(color.b),
      g = this.fuzzColorComponent(color.g)
    )
  }

  // Override

  override def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // Random color based on team
    player.color = this.getTeamColor(player.team)
    // Spawn player
    gameServer.spawnPlayer(player)
  }

  override def onServerInit(gameServer : GameServer) {
    // Set up teams
    this.nodes.indices.foreach(this.nodes(_) = new ArrayBuffer)

    // migrate current players to team mode
    gameServer.clients.map(_.playerTracker).foreach(client => {
      this.onPlayerInit(client)
      client.color = this.getTeamColor(client.team)
      client.cells.foreach(cell => {
        cell.setColor(client.color)
        this.nodes(client.team) += cell
      })
    })
  }

  override def onPlayerInit(player : PlayerTracker) {
    // Get random team
    player.team = Math.floor(Math.random() * this.teamAmount).toInt
  }

  override def onCellAdd(cell : PlayerCell) {
    // Add to team list
    this.nodes(cell.owner.get.getTeam) += cell
  }

  override def onCellRemove(cell : PlayerCell) {
    // Remove from team list
    this.nodes(cell.owner.get.getTeam) -= cell
  }

  override def canCollide(cellOwner : PlayerTracker, checkOwner : PlayerTracker) : Boolean = {
    // Collision with teammates
    cellOwner.getTeam == checkOwner.team
  }

  override def updateLB(gameServer : GameServer) {
    // Get mass
    val teamMass = this.nodes.map(teamNodes => {
      // Loop through cells
      teamNodes.filter(null != _).map(_.mass).sum
    })
    val total = teamMass.sum

    // No players
    if(total > 0) {
      val lb : ArrayBuffer[Float] = teamMass.map(m => (m / total).toFloat)(collection.breakOut)
      gameServer.leaderboard = new TeamLeaderboard(lb)
    }
  }
}