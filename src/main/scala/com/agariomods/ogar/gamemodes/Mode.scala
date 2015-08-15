package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity._
import com.agariomods.ogar.{Color, GameServer, PlayerTracker, Position}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

trait Mode {
  val ID = -1
  val name = "Blank"
  val decayMod = 1.0 // Modifier for decay rate (Multiplier)
  val haveTeams = false // True = gamemode uses teams, false = gamemode doesnt use teams

  val specByLeaderboard = false // false = spectate from player list instead of leaderboard

  var rankOne : PlayerTracker = null // Usually the best player. It's unused really

  // Override these

  def onServerInit(gameServer : GameServer) {
    // Called when the server starts
    gameServer.run = true
  }

  def onTick(gameServer : GameServer) {
    // Called on every game tick
  }

  def onChange(gameServer : GameServer) {
    // Called when someone changes the gamemode via console commands
  }

  def onPlayerInit(player : PlayerTracker) {
    // Called after a player object is constructed
  }

  def onPlayerSpawn(gameServer : GameServer, player : PlayerTracker) {
    // Called when a player is spawned
    player.color = gameServer.getRandomColor // Random color
    gameServer.spawnPlayer(player)
  }

  def pressQ(gameServer : GameServer, player : PlayerTracker) {
    // Called when the Q key is pressed
    if (player.spectate) {
      gameServer.switchSpectator(player)
    }
  }

  def pressW(gameServer : GameServer, player : PlayerTracker) {
    // Called when the W key is pressed
    gameServer.ejectMass(player)
  }

  def pressSpace(gameServer : GameServer, player : PlayerTracker) {
    // Called when the Space bar is pressed
    gameServer.splitCells(player)
  }

  def onCellAdd(cell : PlayerCell) {
    // Called when a player cell is added
  }

  def onCellRemove(cell : PlayerCell) {
    // Called when a player cell is removed
  }

  def onCellMove(x1 : Int, y1 : Int, cell : PlayerCell) {
    // Called when a player cell is moved
  }

  def updateLB(gameServer : GameServer) {
    // Called when the leaderboard update function is called
  }

  def createFood(gameServer : GameServer, position : Position) : Food = {
    new Food(gameServer.getNextNodeId, None, position, gameServer.config.foodMass, gameServer)
  }

  def createVirus(gameServer : GameServer, position : Position) = {
    new Virus(gameServer.getNextNodeId, None, position, gameServer.config.virusStartMass, gameServer)
  }

  def createPlayer(client : PlayerTracker, position : Position, mass : Double) = {
    val gameServer = client.gameServer
    new PlayerCell(gameServer.getNextNodeId, Some(client), position, mass, gameServer)
  }

  def calcMergeTime(cell : HasMergeTime, base : Int) {
    cell.recombineTicks = base + (0.02 * cell.mass).floor.toInt // Int (30 sec + (.02 * mass))
  }

  def getRandomSpawn(gameServer : GameServer) = {
    // Random spawns for players
    var pos : Position = null

    if (gameServer.currentFood > 0) {
      // Spawn from food
      val continueBreak = new Breaks
      val breakBreak = new Breaks

      var i = gameServer.nodes.length - 1
      breakBreak.breakable {
        while (i > -1) {
          continueBreak.breakable {
            // Find random food
            val node = gameServer.nodes(i)

            if(null == node || node.inRange) {
              // Skip if food is about to be eaten/undefined
              continueBreak.break() // continue
            }

            if(node.getType == CellTypes.Food) {
              pos = node.position
              gameServer.removeNode(node)
              breakBreak.break() //break
            }
          }

          i -= 1
        }
      }
    }

    if (null == pos) {
      // Get random spawn if no food cell is found
      pos = gameServer.getRandomPosition
    }

    pos
  }

  def getRandomColor(gameServer : GameServer) = {
    val index = Math.floor(Math.random() * gameServer.colors.length).toInt
    val color = gameServer.colors(index)
    new Color(
      r = color.r,
      b = color.b,
      g = color.g
    )
  }

  def getCellsInRange(cell : PlayerCell) : Traversable[Cell] = {
    val list = new ArrayBuffer[Cell]
    val squareR = cell.getSquareSize // Get cell squared radius
    val owner = cell.owner.get

    // Loop through all cells that are visible to the cell. There is probably a more efficient way of doing this but whatever
    owner.visibleNodes.foreach(check => Breaks.breakable {
      if(null == check) {
        Breaks.break() // continue
      }

      // if something already collided with this cell, don't check for other collisions
      if(check.inRange) {
        Breaks.break() // continue
      }

      // Can't eat itself
      if(cell.nodeId == check.nodeId) {
        Breaks.break() // continue
      }

      // Can't eat cells that have collision turned off
      if((cell.owner == check.owner) && cell.ignoreCollision) {
        Breaks.break() // continue
      }

      // AABB Collision
      if(!check.collisionCheck2(squareR, cell.position)) {
        Breaks.break() // continue
      }

      if(canEat(cell, check)) {
        // Add to list of cells nearby
        list += check

        // Something is about to eat this cell; no need to check for other collisions with it
        check.inRange = true
      }
    })

    list
  }

  final def checkEatingDistance(cell : PlayerCell, check : Cell) = {
    // Eating range
    val xs = Math.pow(check.position.x - cell.position.x, 2)
    val ys = Math.pow(check.position.y - cell.position.y, 2)
    val dist = Math.sqrt(xs + ys)

    val eatingRange = cell.getSize - check.getEatingRange // Eating range = radius of eating cell + 40% of the radius of the cell being eaten
    dist <= eatingRange
  }

  final def checkEatingMass(multiplier : Double, cell : PlayerCell, check : Cell) : Boolean = {
    // Make sure the cell is big enough to be eaten.
    (check.mass * multiplier) <= cell.mass
  }

  final def canEatFood(cell : PlayerCell, check : Cell) = true

  final def canEatVirus(cell : PlayerCell, check : Cell) = checkEatingMass(1.33, cell, check) && checkEatingDistance(cell, check)

  final def canEatOwnCell(cell : PlayerCell, check : Cell) : Boolean = {
    // Can't eat self if it's not time to recombine yet
    if((cell.recombineTicks > 0) || (check.asInstanceOf[PlayerCell].recombineTicks > 0)) {
      return false
    }
    checkEatingMass(1.00, cell, check) && checkEatingDistance(cell, check)
  }

  def canEatTeammate(cell : PlayerCell, check : Cell) : Boolean = {
    // Can't eat team members
    false
  }

  final def canEatPlayer(cell : PlayerCell, check : Cell) : Boolean = {
    checkEatingMass(1.25, cell, check) && checkEatingDistance(cell, check)
  }

  def canEatOther(cell : PlayerCell, check : Cell) : Boolean = {
    checkEatingMass(1.25, cell, check) && checkEatingDistance(cell, check)
  }

  final def canEat(cell : PlayerCell, check : Cell) : Boolean = {
    check.getType match {
      case CellTypes.Food => // Food cell
        canEatFood(cell, check)
      case CellTypes.Virus => // Virus
        canEatVirus(cell, check)
      case CellTypes.Player => // Players
        if(check.owner == cell.owner) {
          canEatOwnCell(cell, check)
        } else if(haveTeams && check.owner.map(_.getTeam) == cell.owner.map(_.getTeam)) {
          canEatTeammate(cell, check)
        } else {
          canEatPlayer(cell, check)
        }
      case _ => // Other
        canEatOther(cell, check)
    }
  }

  def getNearestVirus(gameServer : GameServer, cell : Cell) : FeedableCell = {
    // More like getNearbyVirus
    var virus : Virus = null
    val r = 100 // Checking radius

    val topY = cell.position.y - r
    val bottomY = cell.position.y + r

    val leftX = cell.position.x - r
    val rightX = cell.position.x + r

    // Loop through all viruses on the map. There is probably a more efficient way of doing this but whatever
    Breaks.breakable {
      gameServer.nodesVirus.foreach(check => {
        if(null != check && check.collisionCheck(bottomY, topY, rightX, leftX)) {
          // Add to list of cells nearby
          virus = check

          Breaks.break() // stop checking when a virus found
        }
      })
    }

    virus
  }

  def splitCells(gameServer : GameServer, client : PlayerTracker) {
    val toSplit = client.cells.toArray
    toSplit.foreach(cell => Breaks.breakable {
      if(client.cells.size >= gameServer.config.playerMaxCells) {
        // Player cell limit
        Breaks.break() // continue
      }

      if(null == cell) {
        Breaks.break() // continue
      }

      if(cell.mass < gameServer.config.playerMinMassSplit) {
        Breaks.break() // continue
      }

      // Get individual cell coords if they exist
      var y2 = client.mouse.y
      var x2 = client.mouse.x
      if(client.mouseCells.contains(cell.nodeId)) {
        val specialPos = client.mouseCells(cell.nodeId)
        x2 = specialPos.x
        y2 = specialPos.y
      }

      // Get angle
      val deltaY = y2 - cell.position.y
      val deltaX = x2 - cell.position.x
      val angle = Math.atan2(deltaX, deltaY)

      // Get starting position
      val size = cell.getSize / 2.0
      val startPos = Position.toInt(
        x = cell.position.x + size * Math.sin(angle),
        y = cell.position.y + size * Math.cos(angle)
      )
      // Calculate mass and speed of splitting cell
      val splitSpeed = cell.getSpeed * 6
      val newMass = cell.mass / 2.0
      cell.mass = newMass

      // Create cell
      val split = createPlayer(client, startPos, newMass)
      split.setAngle(angle)
      split.setMoveEngineData(splitSpeed, 32, 0.85)
      gameServer.gameMode.calcMergeTime(split, gameServer.config.playerRecombineTime)

      // Add to moving cells list
      gameServer.setAsMovingNode(split)
      gameServer.addNode(split)
    })
  }

  def newCellVirused(gameServer : GameServer, client : PlayerTracker, parent : PlayerCell, angle : Double, mass : Double, speed : Double) {
    // Starting position
    val startPos = parent.position

    // Create cell
    val newCell = createPlayer(client, startPos, mass)
    newCell.setAngle(angle)
    newCell.setMoveEngineData(speed, 15)
    gameServer.gameMode.calcMergeTime(newCell, gameServer.config.playerRecombineTime)
    newCell.ignoreCollision = true // Remove collision checks

    // Add to moving cells list
    gameServer.addNode(newCell)
    gameServer.setAsMovingNode(newCell)
  }
}
