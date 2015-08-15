package com.agariomods.ogar.gamemodes

import com.agariomods.ogar._
import com.agariomods.ogar.entity._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object GameState {
  val WF_PLAYERS = 0
  val WF_START = 1
  val IN_PROGRESS = 2
}

class TeamZ extends Mode
with ModeWithCollisionDetection
with ModeWithHeroCells
with ModeWithBrainCells
{

  override val ID = 13
  override val name = "Zombie Team"
  override val haveTeams = true

  // configurations:
  val minPlayer = 2
  // game is auto started if there are at least 2 players
  val gameDuration = 18000
  // ticks, 1 tick = 50 ms (20 ticks = 1 s)
  //val warmUpDuration = 600
  val warmUpDuration = 60
  // ticks, time to wait between games
  val spawnBrainInterval = 1200
  // ticks
  val spawnHeroInterval = 600
  // ticks
  val defaultColor = new Color(r = 0x9b, g = 0x30, b = 0xff)

  val maxBrain = -1
  // set this param to any negative number to keep the number of brains not exceed number of humans
  val maxHero = 4 // set this param to any negative number to keep the number of heroes not exceed number of zombies

  // game mode data:
  var state = GameState.WF_PLAYERS
  var winTeam = -1
  var gameTimer = 0
  var zombies = new ArrayBuffer[PlayerTracker]
  // the clients of zombie players
  val humans = new ArrayBuffer[PlayerTracker]
  // the clients of human players

  val heroes = new ArrayBuffer[Hero]
  val brains = new ArrayBuffer[Brain]

  var spawnHeroTimer = 0
  var spawnBrainTimer = 0

  var localLB = new ArrayBuffer[PlayerTracker]

  // Gamemode Specific Functions

  def spawnDrug(gameServer : GameServer, cell : Cell) : Boolean = {
    // spawn HERO or BRAIN
    var max = 0
    var proceedNext = false
    if(cell.getType == CellTypes.Hero) {
      max = if (this.maxHero < 0) this.zombies.length else this.maxHero
      proceedNext = this.heroes.length < max
    }
    else if(cell.getType == CellTypes.Brain) {
      max = if (this.maxBrain < 0) this.humans.length else this.maxBrain
      proceedNext = this.brains.length < max
    }
    if(proceedNext) {
      val pos = gameServer.getRandomPosition

      // Check for players
      val collided = gameServer.nodesPlayer.exists(check => {
        val r = check.getSize // Radius of checking player cell

        // Collision box
        val topY = check.position.y - r
        val bottomY = check.position.y + r
        val leftX = check.position.x - r
        val rightX = check.position.x + r

        // Check for collisions
        if(pos.y > bottomY || pos.y < topY || pos.x > rightX || pos.x < leftX) {
          false
        } else {
          // Collided
          true
        }
      })

      // Spawn if no cells are colliding
      if(!collided) {
        cell.position = pos
        gameServer.addNode(cell)
        return true // SUCCESS with spawn
      }
      return false // FAILED because of collision
    }
    true // SUCCESS without spawn
  }

  // Call to change a human client to a zombie
  def turnToZombie(client : PlayerTracker) {
    client.team = 0 // team Z
    this.createZColorFactor(client)
    this.updateZColor(client, 0x7) // Gray

    // remove from human list
    this.humans -= client

    // add to zombie list
    this.zombies += client
  }

  def boostSpeedCell(cell : PlayerCell) {
    cell.speedMultiplier = 2
  }

  private def boostSpeed(client : PlayerTracker) {
    client.cells.foreach(this.boostSpeedCell)
  }

  def resetSpeedCell(cell : PlayerCell) {
    cell.speedMultiplier = 1
  }

  private def resetSpeed(client : PlayerTracker) {
    client.cells.foreach(this.resetSpeedCell)
  }

  def startGame(gameServer : GameServer) {
    this.humans.foreach(client => {
      client.team = client.pID
      client.crazyTimer = 0
      client.eatenHeroTimer = 0
      client.eatenBrainTimer = 0
      client.color = gameServer.getRandomColor

      client.cells.foreach(cell => {
        if(null != cell) {
          cell.setColor(client.color)
          cell.mass = gameServer.config.playerStartMass
          this.resetSpeedCell(cell)
        }
      })
    })

    // Select random human to be the zombie
    val zombie = this.humans((Math.random() * this.humans.length).floor.toInt)
    this.turnToZombie(zombie)

    this.winTeam = -1
    this.state = GameState.IN_PROGRESS
    this.gameTimer = this.gameDuration
  }

  def endGame(gameServer : GameServer) {

    // reset game
    this.zombies.foreach(client => {
      val index = this.humans.indexOf(client)
      if(index < 0) {
        this.humans += client
      }
    })

    this.zombies = new ArrayBuffer[PlayerTracker]()

    this.spawnHeroTimer = 0
    this.spawnBrainTimer = 0

    localLB = new ArrayBuffer[PlayerTracker]() // reset leader board

    this.humans.foreach(client => {
      client.color = this.defaultColor
      client.team = 1
      client.cells.foreach(cell => {
        cell.setColor(this.defaultColor)
      })
    })

    this.state = GameState.WF_PLAYERS
    this.gameTimer = 0
  }

  // Override

  // Change to AGARIO colorful scheme
  override def getRandomColor(gameServer : GameServer) = {
    val colorRGB = Random.shuffle(Seq(0xFF, 0x07, (Math.random() * 256).floor.toInt))
    new Color(
      r = colorRGB(0),
      b = colorRGB(1),
      g = colorRGB(2)
    )
  }

  override def getNearestVirus(gameServer : GameServer, cell : Cell) : FeedableCell = {
    // More like getNearbyVirus
    val r = 100 // Checking radius

    val topY = cell.position.y - r
    val bottomY = cell.position.y + r

    val leftX = cell.position.x - r
    val rightX = cell.position.x + r

    // loop through all heroes
    val hero = this.heroes.find(check => {
      (null != check) && check.collisionCheck(bottomY, topY, rightX, leftX)
    })

    if(hero.nonEmpty)
      return hero.get

    // loop through all brains
    val brain = this.brains.find(check => {
      (null != check) && check.collisionCheck(bottomY, topY, rightX, leftX)
    })

    if(brain.nonEmpty)
      return brain.get

    super.getNearestVirus(gameServer, cell)
  }

  override def getCellsInRange(cell : PlayerCell) : Traversable[Cell] = {
    if(this.state != GameState.IN_PROGRESS) {
      new ArrayBuffer[Cell]()
    } else {
      super.getCellsInRange(cell)
    }
  }

  override def onServerInit(gameServer : GameServer) {
    super.onServerInit(gameServer)

    // Handle "gamemode" command:
    gameServer.clients.map(_.playerTracker).filter(null != _).foreach(client => {
      if(client.cells.size > 0) {
        client.eatenBrainTimer = 0
        client.eatenHeroTimer = 0
        client.crazyTimer = 0
        client.color = this.defaultColor
        client.team = 1

        client.cells.foreach(cell =>  {
          cell.setColor(this.defaultColor)
        })
        this.humans += client
      }
    })
  }

  override def onChange(gameServer : GameServer) {
    // Called when someone changes the gamemode via console commands
    // remove Brain and Hero
    this.brains.foreach(gameServer.removeNode)
    this.heroes.foreach(gameServer.removeNode)

    // discard all boost:
    this.humans.foreach(client => {
      if(this.isCrazy(client)) {
        this.resetSpeed(client)
      }
    })
    this.zombies.foreach(client => {
      if(this.hasEatenBrain(client)) {
        this.resetSpeed(client)
      }
    })
  }

  override def onTick(gameServer : GameServer) {
    // Called on every game tick

    this.state match {
      case GameState.WF_PLAYERS =>
        if(this.humans.length >= this.minPlayer) {
          this.state = GameState.WF_START
          this.gameTimer = this.warmUpDuration
        }
      case GameState.WF_START =>
        this.gameTimer -= 1
        if(this.gameTimer == 0) {
          if(this.humans.length >= this.minPlayer) {
            // proceed:
            this.startGame(gameServer)
          }
          else {
            // back to previous state:
            this.state = GameState.WF_PLAYERS
          }
        }

      case GameState.IN_PROGRESS =>
        this.gameTimer -= 1
        if(this.gameTimer == 0) {
          // human wins
          this.winTeam = 1
        }
        else {
          if(this.humans.length == 0) {
            // no human left
            // zombie wins
            this.winTeam = 0
          }
          else if(this.zombies.length == 0) {
            // no zombie left
            // human wins
            this.winTeam = 1
          }
        }

        if(this.winTeam >= 0) {
          this.endGame(gameServer)
        }
    }

    // change color of zombies
    this.zombies.foreach(tickZombieColor)

    this.humans.map(identity).foreach(tickHeroColor)

    // check timer to spawn Hero:
    this.spawnHeroTimer += 1
    if(this.spawnHeroTimer >= this.spawnHeroInterval) {
      this.spawnHeroTimer = 0
      val cell = new Hero(gameServer.getNextNodeId, None, Position.Zero, 0, gameServer)
      while (!this.spawnDrug(gameServer, cell)) {} // collision detect algorithm needs enhancement
    }

    // check timer to spawn Brain:
    this.spawnBrainTimer += 1
    if(this.spawnBrainTimer >= this.spawnBrainInterval) {
      this.spawnBrainTimer = 0
      val cell = new Brain(gameServer.getNextNodeId, None, Position.Zero, 0, gameServer)
      while (!this.spawnDrug(gameServer, cell)) {} // collision detect algorithm needs enhancement
    }
  }

  override def onCellAdd(cell : PlayerCell) {
    // Called when a player cell is added
    val client = cell.owner.get
    if(client.cells.size == 1) {
      // first cell
      client.team = client.pID
      client.color = new Color(cell.color)
      client.eatenBrainTimer = 0
      client.eatenHeroTimer = 0
      client.crazyTimer = 0
      this.humans += client

      if(this.state == GameState.IN_PROGRESS) {
        this.turnToZombie(client)
      } else {
        client.color = this.defaultColor
        cell.setColor(this.defaultColor)
        client.team = 1 // game not started yet
      }
    }
  }

  override def onCellRemove(cell : PlayerCell) {
    // Called when a player cell is removed
    val client = cell.owner.get
    if(client.cells.size == 0) {
      // last cell
      if(isZombie(client)) {
        // Z team
        this.zombies -= client
      } else {
        // H team
        this.humans -= client
      }
    }
  }

  def isZombie(client : PlayerTracker) = client.getTeam == 0

  override def canCollide(cellOwner : PlayerTracker, checkOwner : PlayerTracker) : Boolean = {
    if(this.hasEatenHero(checkOwner) || this.hasEatenHero(cellOwner)) {
      return false // continue
    }

    // Collision with zombies
    isZombie(checkOwner) || isZombie(cellOwner)
  }

  override def onCollision(cellOwner : PlayerTracker, checkOwner : PlayerTracker): Unit = {
    var crazyClient : PlayerTracker = null
    if(isZombie(checkOwner) && !isZombie(cellOwner)) {
      crazyClient = cellOwner
    } else if(isZombie(cellOwner) && !isZombie(checkOwner)) {
      crazyClient = checkOwner
    }

    if(crazyClient != null && !this.isCrazy(crazyClient)) {
      crazyClient.crazyTimer = this.crazyDuration
      crazyClient.colorToggle = 0
      this.boostSpeed(crazyClient)
    }
  }

  override def updateLB(gameServer : GameServer) {
    val lb = new ArrayBuffer[String]()

    if(this.winTeam == 0) {
      lb += "ZOMBIE WINS"
      lb += "_______________"
    }
    else if(this.winTeam > 0) {
      lb += "HUMAN WINS"
      lb += "_______________"
    }

    this.state match {
      case GameState.WF_PLAYERS =>
        lb += "WAITING FOR"
        lb += "PLAYERS..."
        lb += this.humans.length + "/" + this.minPlayer

      case GameState.WF_START =>
        lb += "GAME STARTS IN :"
        val min = (this.gameTimer / 20 / 60).floor.toInt
        val sec = ((this.gameTimer / 20) >> 0) % 60
        lb += (if(min < 10) "0" else "") + min + ":" + (if(sec < 10) "0" else "") + sec

      case GameState.IN_PROGRESS =>
        val min = (this.gameTimer / 20 / 60).floor.toInt
        val sec = ((this.gameTimer / 20) >> 0) % 60
        lb += (if(min < 10) "0" else "") + min + ":" + (if(sec < 10) "0" else "") + sec
        lb += "HUMAN : " + this.humans.length
        lb += "ZOMBIE : " + this.zombies.length
        lb += "_______________"

        import com.agariomods.ogar.utils.IterExt._

        // Loop through all clients
        localLB = gameServer.clients.filter(null != _).map(_.playerTracker).filter(!isZombie(_)).filter(_.cells.size > 0).getTop(6, (recalc : Boolean) => (pl : PlayerTracker) => pl.getScore(recalc))

        localLB.map(_.getName).foreach(name => {
          if(lb.size < 10) {
            lb += name
          }
        })

      case _ =>
        lb += "ERROR STATE"
    }

    gameServer.leaderboard = new TextLeaderboard(lb)
  }
}

