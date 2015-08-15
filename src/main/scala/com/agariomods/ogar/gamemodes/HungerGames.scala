package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.{Cell, CellTypes, Virus}
import com.agariomods.ogar.{GameServer, PlayerTracker, Position}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class HungerGames extends Tournament {

  override val ID = 11
  override val name = "Hunger Games"

  // Gamemode Specific Variables
  //override var maxContenders = 12
  val baseSpawnPoints = Array(
    new Position(1600, 200), new Position(3200, 200), new Position(4800, 200), // Top
    new Position(200, 1600), new Position(200, 3200), new Position(200, 4800), // Left
    new Position(6200, 1600), new Position(6200, 3200), new Position(6200, 4800), // Right
    new Position(1600, 6200), new Position(3200, 6200), new Position(4800, 6200) // Bottom
  )

  var contenderSpawnPoints : ArrayBuffer[Position] = baseSpawnPoints.map(identity)(collection.breakOut)
  val borderDec = 100 // Border shrinks by this size everytime someone dies

  // Gamemode Specific Functions

  def getPos = {
    var pos = Position.Zero

    // Random Position
    if(this.contenderSpawnPoints.length > 0) {
      val index = Math.floor(Math.random() * this.contenderSpawnPoints.length).toInt
      pos = this.contenderSpawnPoints(index)
      this.contenderSpawnPoints.remove(index)
    }

    pos
  }

  def spawnFood(gameServer : GameServer, mass : Double, pos : Position) {
    gameServer.spawnFood(pos, Some(mass))
  }

  def spawnVirus(gameServer : GameServer, pos : Position) {
    val v = new Virus(gameServer.getNextNodeId, None, pos, gameServer.config.virusStartMass, gameServer)
    gameServer.addNode(v)
  }

  override def onPlayerDeath(gameServer : GameServer) {
    val config = gameServer.config
    config.borderLeft += this.borderDec
    config.borderRight -= this.borderDec
    config.borderTop += this.borderDec
    config.borderBottom -= this.borderDec

    // Remove all cells

    val tmp : Array[Cell] = gameServer.nodes.map(identity)(collection.breakOut)
    tmp.foreach(node => Breaks.breakable {
      if((null == node) || (node.getType == CellTypes.Player)) {
        Breaks.break() // continue
      }

      // Move
      if(node.position.x < config.borderLeft || node.position.x > config.borderRight || node.position.y < config.borderTop || node.position.y > config.borderBottom) {
        gameServer.removeNode(node)
      }
    })

    super.onPlayerDeath(gameServer)
  }

  // Override

  override def onServerInit(gameServer : GameServer) {
    // Prepare
    this.prepare(gameServer)

    // Resets spawn points
    this.contenderSpawnPoints = this.baseSpawnPoints.map(identity)(collection.breakOut)

    // Override config values
    if(gameServer.config.serverBots > this.maxContenders) {
      // The number of bots cannot exceed the maximum amount of contenders
      gameServer.config.serverBots = this.maxContenders
    }
    gameServer.config.spawnInterval = 20
    gameServer.config.borderLeft = 0
    gameServer.config.borderRight = 6400
    gameServer.config.borderTop = 0
    gameServer.config.borderBottom = 6400
    gameServer.config.foodSpawnAmount = 5 // This is hunger games
    gameServer.config.foodStartAmount = 100
    gameServer.config.foodMaxAmount = 200
    gameServer.config.foodMass = 2 // Food is scarce, but its worth more
    gameServer.config.virusMinAmount = 10 // We need to spawn some viruses in case someone eats them all
    gameServer.config.virusMaxAmount = 100
    gameServer.config.ejectSpawnPlayer = 0
    gameServer.config.playerDisconnectTime = 10 // So that people dont disconnect and stall the game for too long

    // Spawn Initial Virus/Large food
    val mapWidth = gameServer.config.borderRight - gameServer.config.borderLeft
    val mapHeight = gameServer.config.borderBottom - gameServer.config.borderTop

    // Food
    this.spawnFood(gameServer, 200, Position.toInt(
      x = mapWidth * 0.5, y = mapHeight * 0.5
    )) // Center
    this.spawnFood(gameServer, 80, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.6
    )) //
    this.spawnFood(gameServer, 80, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.6
    ))
    this.spawnFood(gameServer, 80, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.4
    ))
    this.spawnFood(gameServer, 80, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.4
    ))
    this.spawnFood(gameServer, 50, Position.toInt(
      x = mapWidth * 0.7, y = mapHeight * 0.5
    )) //
    this.spawnFood(gameServer, 50, Position.toInt(
      x = mapWidth * 0.3, y = mapHeight * 0.5
    ))
    this.spawnFood(gameServer, 50, Position.toInt(
      x = mapWidth * 0.5, y = mapHeight * 0.7
    ))
    this.spawnFood(gameServer, 50, Position.toInt(
      x = mapWidth * 0.5, y = mapHeight * 0.3
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.7, y = mapHeight * 0.625
    )) // Corner
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.625, y = mapHeight * 0.7
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.3, y = mapHeight * 0.4
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.3
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.3
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.7, y = mapHeight * 0.4
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.3, y = mapHeight * 0.6
    ))
    this.spawnFood(gameServer, 30, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.7
    ))

    // Virus
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.5
    )) //
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.5
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.5, y = mapHeight * 0.4
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.5, y = mapHeight * 0.6
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.3, y = mapHeight * 0.3
    )) //
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.3, y = mapHeight * 0.7
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.7, y = mapHeight * 0.3
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.7, y = mapHeight * 0.7
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.25, y = mapHeight * 0.6
    )) //
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.25, y = mapHeight * 0.4
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.75, y = mapHeight * 0.6
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.75, y = mapHeight * 0.4
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.25
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.25
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.6, y = mapHeight * 0.75
    ))
    this.spawnVirus(gameServer, Position.toInt(
      x = mapWidth * 0.4, y = mapHeight * 0.75
    ))
  }

  override def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // Only spawn players if the game hasnt started yet
    if((this.gamePhase == 0) && (this.contenders.length < this.maxContenders)) {
      player.color = gameServer.getRandomColor // Random color
      this.contenders += player // Add to contenders list
      gameServer.spawnPlayer(player, this.getPos) // Spawn in one of the predefined positions

      if(this.contenders.length == this.maxContenders) {
        // Start the game once there is enough players
        this.startGamePrep(gameServer)
      }
    }
  }
}