package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.ai.BotPlayer
import com.agariomods.ogar.entity.PlayerCell
import com.agariomods.ogar.{GameServer, PlayerTracker, TextLeaderboard}

import scala.collection.mutable.ArrayBuffer

class Tournament extends Mode {
  override val ID = 10
  override val name = "Tournament"

  // Config (1 tick = 1000 ms)
  var prepTime = 5
  // Amount of ticks after the server fills up to wait until starting the game
  var endTime = 15
  // Amount of ticks after someone wins to restart the game
  var autoFill = false
  var autoFillPlayers = 1
  var dcTime = 0

  // Gamemode Specific Variables
  var gamePhase = 0
  // 0 = Waiting for players, 1 = Prepare to start, 2 = Game in progress, 3 = End
  val contenders = new ArrayBuffer[PlayerTracker]()
  var maxContenders = 12

  var winner : PlayerTracker = null
  var timer = 0
  var timeLimit = 3600 // in seconds

  // Gamemode Specific Functions

  def startGamePrep(gameServer : GameServer) {

    this.gamePhase = 1
    this.timer = this.prepTime // 10 seconds
  }

  def startGame(gameServer : GameServer) {
    gameServer.run = true
    this.gamePhase = 2
    this.getSpectate // Gets a random person to spectate
    gameServer.config.playerDisconnectTime = this.dcTime // Reset config
  }

  def endGame(gameServer : GameServer) {
    this.winner = this.contenders(0)
    this.gamePhase = 3
    this.timer = this.endTime // 30 Seconds
  }

  def endGameTimeout(gameServer : GameServer) {
    gameServer.run = false
    this.gamePhase = 4
    this.timer = this.endTime // 30 Seconds
  }

  def fillBots(gameServer : GameServer) {
    // Fills the server with bots if there arent enough players
    val fill = this.maxContenders - this.contenders.length
    (1 to fill).foreach(_ => gameServer.bots.addBot())
  }

  def getSpectate = {
    // Finds a random person to spectate
    val index = Math.floor(Math.random() * this.contenders.length).toInt

    this.rankOne = this.contenders(index)
    this.rankOne
  }

  def prepare(gameServer : GameServer) {
    // Remove all cells

    while(gameServer.nodes.nonEmpty) {
      val node = gameServer.nodes.head
      gameServer.removeNode(node)
    }

    gameServer.bots.loadNames()

    // Pauses the server
    gameServer.run = false
    this.gamePhase = 0

    // Get config values
    if(gameServer.config.tourneyAutoFill > 0) {
      this.timer = gameServer.config.tourneyAutoFill
      this.autoFill = true
      this.autoFillPlayers = gameServer.config.tourneyAutoFillPlayers
    }

    // Handles disconnections
    this.dcTime = gameServer.config.playerDisconnectTime
    gameServer.config.playerDisconnectTime = 0
    gameServer.config.playerMinMassDecay = gameServer.config.playerStartMass

    this.prepTime = gameServer.config.tourneyPrepTime
    this.endTime = gameServer.config.tourneyEndTime
    this.maxContenders = gameServer.config.tourneyMaxPlayers

    // Time limit
    this.timeLimit = gameServer.config.tourneyTimeLimit * 60 // in seconds
  }

  def onPlayerDeath(gameServer : GameServer) {
    // Nothing
  }

  def formatTime(time : Int) : String = {
    if(time < 0) {
      return "0:00"
    }
    // Format
    val min = Math.floor(this.timeLimit / 60)
    val sec = this.timeLimit % 60
    val secS = if(sec > 9) sec.toString else "0" + sec.toString
    min + ":" + secS
  }

  // Override

  override def onServerInit(gameServer : GameServer) {
    this.prepare(gameServer)
  }

  override def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // Only spawn players if the game hasnt started yet
    if((this.gamePhase == 0) && (this.contenders.length < this.maxContenders)) {
      player.color = gameServer.getRandomColor // Random color
      this.contenders += player // Add to contenders list
      gameServer.spawnPlayer(player)

      if(this.contenders.length == this.maxContenders) {
        // Start the game once there is enough players
        this.startGamePrep(gameServer)
      }
    }
  }

  override def onCellRemove(cell : PlayerCell) {
    val owner = cell.owner.get
    var human_just_died = false

    if(owner.cells.size <= 0) {
      // Remove from contenders list
      val index = this.contenders.indexOf(owner)
      if(index != -1) {
        if(!owner.isInstanceOf[BotPlayer]) {
          human_just_died = true
        }
        this.contenders.remove(index)
      }

      // Victory conditions
      val humans = this.contenders.count(!_.isInstanceOf[BotPlayer])

      // the game is over if:
      // 1) there is only 1 player left, OR
      // 2) all the humans are dead, OR
      // 3) the last-but-one human just died
      if((this.contenders.length == 1 || humans == 0 || (humans == 1 && human_just_died)) && this.gamePhase == 2) {
        this.endGame(owner.gameServer)
      } else {
        // Do stuff
        this.onPlayerDeath(owner.gameServer)
      }
    }
  }

  override def updateLB(gameServer : GameServer) {
    //val lb = gameServer.leaderboard.asInstanceOf[PlayerLeaderboard].leaders
    val lb : ArrayBuffer[String] = new ArrayBuffer[String]

    this.gamePhase match {
      case 0 =>
        lb += "Waiting for"
        lb += "players: "
        lb += this.contenders.length + "/" + this.maxContenders
        if(this.autoFill) {
          if(this.timer <= 0) {
            this.fillBots(gameServer)
          } else if(this.contenders.length >= this.autoFillPlayers) {
            this.timer -= 1
          }
        }

      case 1 =>
        lb += "Game starting in"
        lb += this.timer.toString
        lb += "Good luck!"
        if(this.timer <= 0) {
          // Reset the game
          this.startGame(gameServer)
        } else {
          this.timer -= 1
        }

      case 2 =>
        lb += "Players Remaining"
        lb += this.contenders.length + "/" + this.maxContenders
        lb += "Time Limit:"
        lb += this.formatTime(this.timeLimit)
        if(this.timeLimit < 0) {
          // Timed out
          this.endGameTimeout(gameServer)
        } else {
          this.timeLimit -= 1
        }

      case 3 =>
        lb += "Congratulations"
        lb += this.winner.getName
        lb += "for winning!"
        if(this.timer <= 0) {
          // Reset the game
          this.onServerInit(gameServer)
          // Respawn starting food
          gameServer.startingFood()
        } else {
          lb += "Game restarting in"
          lb += this.timer.toString
          this.timer -= 1
        }

      case 4 =>
        lb += "Time Limit"
        lb += "Reached!"
        if(this.timer <= 0) {
          // Reset the game
          this.onServerInit(gameServer)
          // Respawn starting food
          gameServer.startingFood()
        } else {
          lb += "Game restarting in"
          lb += this.timer.toString
          this.timer -= 1
        }
      case _ =>
    }

    gameServer.leaderboard = new TextLeaderboard(lb)
  }
}