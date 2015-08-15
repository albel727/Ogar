package com.agariomods.ogar

import java.io.FileNotFoundException
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import com.agariomods.ogar.ai.{BotLoader, BotPlayer}
import com.agariomods.ogar.entity._
import com.agariomods.ogar.gamemodes.Mode
import com.agariomods.ogar.modules.{CommandList, IniReflect}
import com.agariomods.ogar.net.OgarSocket
import com.agariomods.ogar.packet.{AddNode, Packet, UpdateLeaderboard}
import grizzled.config.Configuration
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.io.Source
import scala.util.control.Breaks
import scala.util.{Failure, Success}

sealed trait Leaderboard {
  def length : Int
}
final case class PlayerLeaderboard(leaders: ArrayBuffer[PlayerTracker] = new ArrayBuffer[PlayerTracker]()) extends Leaderboard {
  override def length = leaders.size
}
final case class TeamLeaderboard(teams: ArrayBuffer[Float] = new ArrayBuffer[Float]()) extends Leaderboard {
  override def length = teams.size
}
final case class TextLeaderboard(lines: ArrayBuffer[String] = new ArrayBuffer[String]()) extends Leaderboard {
  override def length = lines.size
}

// GameServer implementation
class GameServer {
  // Startup
  var run = true
  var lastNodeId = 1
  var lastPlayerId = 1
  var clients = new ArrayBuffer[OgarSocket]()
  var nodes = new ArrayBuffer[Cell]()
  var nodesVirus : collection.mutable.Set[Virus] = mutable.Set.empty // Virus nodes
  var nodesEjected : collection.mutable.Set[Cell] = mutable.Set.empty // Ejected mass nodes
  var nodesPlayer : collection.mutable.Set[PlayerCell] = mutable.Set.empty // Nodes controlled by players

  var currentFood = 0
  val movingNodes : collection.mutable.Set[Cell] = mutable.Set.empty // For move engine
  var leaderboard : Leaderboard = new PlayerLeaderboard()
  var fixedLeaderboard : Option[Leaderboard] = None
  var lb_packet : Packet = null // Leaderboard packet

  val bots = new BotLoader(this)

  var commands = CommandList() // Command handler

  var stats = new ServerStats()

  // Main loop tick
  var time = compat.Platform.currentTime
  var startTime = this.time
  var tick = 0l // 1 second ticks of mainLoop
  var tickMain = 0 // 50 ms ticks, 20 of these = 1 leaderboard update
  var tickSpawn = 0 // Used with spawning food

  // Config
  var config = new GameConfig

  // Gamemodes
  var gameMode : Mode = null

  // Colors
  var colors = Array(
    new Color(235, 75, 0),
    new Color(225, 125, 255),
    new Color(180, 7, 20),
    new Color(80, 170, 240),
    new Color(180, 90, 135),
    new Color(195, 240, 0),
    new Color(150, 18, 255),
    new Color(80, 245, 0),
    new Color(165, 25, 0),
    new Color(80, 145, 0),
    new Color(80, 170, 240),
    new Color(55, 92, 255)
  )

  def start(actorSystem : ActorSystem) {
    gameMode = gamemodes.get(this.config.serverGamemode)

    // Gamemode configurations
    this.gameMode.onServerInit(this)

    // Spawn starting food
    this.startingFood()

    // Start Main Loop
    scheduleLoop(actorSystem)(Duration(1, TimeUnit.MILLISECONDS))({
      mainLoop(actorSystem)
    })

    // Player bots (Experimental)
    if (this.config.serverBots > 0) {
      (1 to this.config.serverBots).foreach(_ => this.bots.addBot())
      GameServer.logger.info("[Game] Loaded {} player bots", this.config.serverBots)
    }

    // Do not refresh stats if the port is negative
    if (this.config.serverStatsPort > 0) {
      GameServer.logger.info("Initiated stats periodic refresh")

      // One-off initial update
      updateStats()

      scheduleLoop(actorSystem)(FiniteDuration(this.config.serverStatsUpdate, TimeUnit.SECONDS))({
        updateStats()
      })
    }
  }

  def getMode = this.gameMode

  def getNextNodeId : Int = {
    // Resets integer
    if (this.lastNodeId > 2147483647) {
      this.lastNodeId = 0
    }
    this.lastNodeId += 1
    this.lastNodeId
  }

  def getNewPlayerID = {
    // Resets integer
    if (this.lastPlayerId > 2147483647) {
      this.lastPlayerId = 0
    }
    this.lastPlayerId += 1
    this.lastPlayerId
  }

  def getRandomPosition = {
    new Position(
      x = Math.floor(Math.random() * (this.config.borderRight - this.config.borderLeft)).toInt + this.config.borderLeft,
      y = Math.floor(Math.random() * (this.config.borderBottom - this.config.borderTop)).toInt + this.config.borderTop
    )
  }

  def getRandomSpawn = this.gameMode.getRandomSpawn(this)

  def getRandomColor = this.gameMode.getRandomColor(this)

  def addNode(node : Cell) {
    this.nodes += node

    // Adds to the owning player's screen
    node.owner.foreach(owner => {
      node.setColor(owner.color)
      owner.cells += node.asInstanceOf[PlayerCell]
      owner.socket.sendPacket(new AddNode(node))
    })

    // Special on-add actions
    node.onAdd(this)

    // Add to visible nodes
    // client.nodeAdditionQueue is only used by human players, not bots
    // for bots it just gets collected forever, using ever-increasing amounts of memory
    this.clients.map(_.playerTracker).filter(client => null != client && !client.isInstanceOf[BotPlayer] && node.visibleCheck(client.viewBox, client.centerPos)).foreach(client => {
      client.nodeAdditionQueue += node
    })
  }

  def removeNode(node : Cell) {
    // Remove from main nodes list
    this.nodes -= node

    // Remove from moving cells list
    this.movingNodes -= node

    // Special on-remove actions
    node.onRemove(this)

    // Animation when eating
    this.clients.map(_.playerTracker).filter(null != _).foreach(client => {
      // Remove from client
      client.nodeDestroyQueue += node
    })
  }

  def cellTick() {
    // Move cells
    this.updateMoveEngine()
  }

  def spawnTick() {
    // Spawn food
    this.tickSpawn += 1
    if (this.tickSpawn >= this.config.spawnInterval) {
      this.updateFood() // Spawn food
      this.virusCheck() // Spawn viruses

      this.tickSpawn = 0 // Reset
    }
  }

  def gamemodeTick() {
    // Gamemode tick
    this.gameMode.onTick(this)
  }

  def cellUpdateTick() {
    // Update cells
    this.updateCells()
  }

  def mainLoop(actorSystem : ActorSystem) {
    val schedule = scheduleOnce(actorSystem)(Duration.Zero) _

    // Timer
    val local = compat.Platform.currentTime
    this.tick += (local - this.time)
    this.time = local

    if (this.tick >= 50) {
      // Loop main functions
      if (this.run) {
        schedule {
          this.cellTick()
        }
        schedule {
          this.spawnTick()
        }
        schedule {
          this.gamemodeTick()
        }
      }

      // Update the client's maps
      this.updateClients()

      // Update cells/leaderboard loop
      this.tickMain += 1
      if (this.tickMain >= 20) { // 1 Second
        schedule {
          this.cellUpdateTick()
        }

        // Update leaderboard with the gamemode's method
        this.leaderboard = new PlayerLeaderboard()
        this.gameMode.updateLB(this)
        this.lb_packet = new UpdateLeaderboard(fixedLeaderboard.getOrElse(this.leaderboard))

        this.tickMain = 0 // Reset
      }

      // Debug
      //out.println(this.tick - 50);

      // Reset
      this.tick = 0
    }
  }

  def updateClients() {
    this.clients.filter(null != _).foreach(client => {
      client.playerTracker.update()
    })
  }

  def startingFood() {
    // Spawns the starting amount of food cells
    (1 to this.config.foodStartAmount).foreach(_ => {
      this.spawnFood()
    })
  }

  def updateFood() {
    val toSpawn = Math.min(this.config.foodSpawnAmount, this.config.foodMaxAmount - this.currentFood)
    (1 to toSpawn).foreach(_ => {
      this.spawnFood()
    })
  }

  def spawnFood(pos : Position = this.getRandomPosition, mass : Option[Double] = None) {
    val f = this.gameMode.createFood(this, pos)
    f.setColor(this.getRandomColor)
    mass.foreach(f.mass = _)

    this.addNode(f)
  }

  def spawnPlayer(player : PlayerTracker, pos : Position = this.getRandomSpawn, mass : Double = config.playerStartMass) {
    // Spawn player and add to world
    val cell = this.gameMode.createPlayer(player, pos, mass)
    this.addNode(cell)

    // Set initial mouse coords
    player.mouse = pos
  }

  def spawnVirus(pos : Position = this.getRandomPosition, mass : Option[Double] = None): Unit = {
    val v = this.gameMode.createVirus(this, pos)
    mass.foreach(v.mass = _)

    this.addNode(v)
  }

  def virusCheck() {
    // Checks if there are enough viruses on the map
    if (this.nodesVirus.size < this.config.virusMinAmount) {
      // Spawns a virus
      val pos = this.getRandomPosition
      val virusSquareSize = (this.config.virusStartMass * 100).floor.toInt

      // Check for players
      val collides = this.nodesPlayer.filterNot(_.mass < this.config.virusStartMass).exists(check => {
        val squareR = check.getSquareSize // squared Radius of checking player cell

        val dx = check.position.x - pos.x
        val dy = check.position.y - pos.y

        dx * dx + dy * dy + virusSquareSize <= squareR // true if Collided
      })

      if(!collides) {
        // Spawn if no cells are colliding
        spawnVirus(pos)
      }
    }
  }

  def updateMoveEngine() {
    // Move player cells
    val toRemove = new mutable.HashSet[Cell]

    // Do not move cells that have already been eaten or have collision turned off
    this.nodesPlayer.filter(null != _).foreach(cell => {
      val client = cell.owner.get

      val mouseTarget = client.mouseCells.getOrElse(cell.nodeId, client.mouse)

      cell.calcMove(mouseTarget, this)

      // Check if cells nearby
      val list = this.getCellsInRange(cell)
      list.foreach(check => {
        // Consume effect
        check.onConsume(cell, this)

        // Remove cell
        check.setKiller(cell)

        // if we're deleting from this.nodesPlayer, fix outer loop variables; we need to update its length, and maybe 'i' too
        if (check.cellType == CellTypes.Player) {
          this.removeNode(check)
        } else {
          toRemove += check
        }
      })
    })
    toRemove.foreach(this.removeNode)

    // A system to move cells not controlled by players (ex. viruses, ejected mass)

    this.movingNodes --= this.movingNodes.filterNot(check => {
      if(null == check) {
        false
      } else if (check.moveEngineTicks > 0) {
        check.onAutoMove(this)
        // If the cell has enough move ticks, then move it
        check.calcMovePhys(this.config)
        true
      } else {
        // Auto move is done
        check.moveDone(this)
        // Remove cell from list
        false
      }
    })
  }

  def setAsMovingNode(node : Cell) {
    this.movingNodes += node
  }

  def splitCells(client : PlayerTracker) = this.gameMode.splitCells(this, client)

  def ejectMass(client : PlayerTracker) {
    client.cells.foreach(cell => {
      Breaks.breakable {

        if(null == cell) {
          Breaks.break() // continue
        }

        if(cell.mass < this.config.playerMinMassEject) {
          Breaks.break() // continue
        }

        val mouseTarget = client.mouseCells.getOrElse(cell.nodeId, client.mouse)

        val deltaX = mouseTarget.x - cell.position.x
        val deltaY = mouseTarget.y - cell.position.y
        var angle = Math.atan2(deltaX, deltaY)

        // Get starting position
        val size = cell.getSize + 5
        val startPos = new Position(
          x = cell.position.x + ((size + this.config.ejectMass) * Math.sin(angle)).toInt,
          y = cell.position.y + ((size + this.config.ejectMass) * Math.cos(angle)).toInt
        )

        // Remove mass from parent cell
        cell.mass -= this.config.ejectMassLoss
        // Randomize angle
        angle += (Math.random() * .4) - 0.2

        // Create cell
        val ejected = new EjectedMass(this.getNextNodeId, None, startPos, this.config.ejectMass, null)
        ejected.setAngle(angle)
        ejected.setMoveEngineData(this.config.ejectSpeed, 20)
        ejected.setColor(cell.getColor)

        this.addNode(ejected)
        this.setAsMovingNode(ejected)
      }
    })
  }

  def newCellVirused(client : PlayerTracker, parent : PlayerCell, angle : Double, mass : Double, speed : Double) = this.gameMode.newCellVirused(this, client, parent, angle, mass, speed)

  def shootVirus(parent : Virus) {
    val newVirus = this.gameMode.createVirus(this, parent.position)
    newVirus.setAngle(parent.getAngle)
    newVirus.setMoveEngineData(200, 20)

    // Add to moving cells list
    this.addNode(newVirus)
    this.setAsMovingNode(newVirus)
  }

  def getCellsInRange(cell : PlayerCell) : Traversable[Cell] = this.gameMode.getCellsInRange(cell)

  def getNearestVirus(cell : Cell) = this.gameMode.getNearestVirus(this, cell)

  def updateCells() {
    if (!this.run) {
      // Server is paused
      return
    }

    // Loop through all player cells
    val massDecay = 1 - (this.config.playerMassDecayRate * this.gameMode.decayMod)
    this.nodesPlayer.filter(null != _).foreach(cell => {
      if (cell.recombineTicks > 0) {
        // Recombining
        cell.recombineTicks -= 1
      }

      // Mass decay
      if (cell.mass >= this.config.playerMinMassDecay) {
        cell.mass *= massDecay
      }
    })
  }

  def loadConfig(fileName : String, gameConfig : GameConfig = this.config, section : String = "Server") = {
    val CommentPattern = """^\s*(//.*)$""".r

    // Load the contents of the config file
    try {
      val load = Configuration(
        Source.fromFile(fileName, "UTF-8"),
        Configuration.DefaultSectionNamePattern,
        CommentPattern
      ).fold(Left(_), ini_cfg => Right(IniReflect.setFromConfiguration(gameConfig, ini_cfg, section)))
      Success(load)
    } catch {
      case e : FileNotFoundException =>
        Failure(e)
    }
  }

  def switchSpectator(player : PlayerTracker) {
    if (this.gameMode.specByLeaderboard) {
      player.spectatedPlayer += 1
      if (player.spectatedPlayer == this.leaderboard.length) {
        player.spectatedPlayer = 0
      }
    } else {
      // Find next non-spectator with cells in the client list
      var oldPlayer = player.spectatedPlayer + 1
      var count = 0
      Breaks.breakable {
        while (player.spectatedPlayer != oldPlayer && count != this.clients.length) {
          if(oldPlayer == this.clients.length) {
            oldPlayer = 0
          } else {
            if(null == this.clients(oldPlayer)) {
              // Break out of loop in case client tries to spectate an undefined player
              player.spectatedPlayer = -1
              Breaks.break()
            }

            if(this.clients(oldPlayer).playerTracker.cells.size > 0) {
              Breaks.break()
            }

            oldPlayer += 1
            count += 1
          }
        }
      }
      if (count == this.clients.length) {
        player.spectatedPlayer = -1
      } else {
        player.spectatedPlayer = oldPlayer
      }
    }
  }

  private def updateStats() {
    val players = this.clients.count(client => {
      null != client && null != client.playerTracker && client.playerTracker.cells.size > 0
    })

    // Create stats
    stats = new ServerStats(
      current_players = this.clients.size,
      alive = players,
      spectators = this.clients.size - players,
      max_players = this.config.serverMaxConnections,
      gamemode = this.gameMode.name,
      start_time = this.startTime
    )
  }

  def getStats = stats

  private def scheduleOnce(actorSystem: ActorSystem)(delay : FiniteDuration)(block : => Unit): Unit = {
    try {
      actorSystem.scheduler.scheduleOnce(delay)({
        synchronized {
          block
        }
      })(actorSystem.dispatcher)
    } catch {
      case i : IllegalStateException if i.getMessage == "cannot enqueue after timer shutdown" =>
      /* ignore */
    }
  }

  private def scheduleLoop(actorSystem : ActorSystem)(duration : FiniteDuration)(body : => Unit) {
    scheduleOnce(actorSystem)(duration)({
      body

      scheduleLoop(actorSystem)(duration)(body)
    })
  }
}

object GameServer {
  val logger = LoggerFactory.getLogger(classOf[GameServer])
}