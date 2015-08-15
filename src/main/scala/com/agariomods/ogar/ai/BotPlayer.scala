package com.agariomods.ogar.ai

import com.agariomods.ogar.entity.{Cell, CellTypes, PlayerCell}
import com.agariomods.ogar.net.OgarSocket
import com.agariomods.ogar.{GameServer, PlayerTracker, Position}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class BotPlayer(_gameServer : GameServer, _socket : OgarSocket) extends PlayerTracker(_gameServer, _socket) {
  //this.color = gameServer.getRandomColor();

  // AI only
  var gameState = 0

  val path = new ArrayBuffer[Position]

  var predators = new ArrayBuffer[Cell]

  // List of cells that can eat this bot
  var threats = new ArrayBuffer[Cell]

  // List of cells that can eat this bot but are too far away
  var prey = new ArrayBuffer[Cell]

  // List of cells that can be eaten by this bot
  var food = new ArrayBuffer[Cell]

  var foodImportant = new ArrayBuffer[Cell]

  // Not used - Bots will attempt to eat this regardless of nearby prey/predators
  var virus = new ArrayBuffer[Cell]

  // List of viruses

  var juke = false

  var target : Cell = null
  var targetVirus : Cell = null
  // Virus used to shoot into the target

  val ejectMass = 0
  // Amount of times to eject mass
  val oldPos = Position.Zero

  // Functions

  def getLowestCell : PlayerCell = {
    // Gets the cell with the lowest mass
    if(this.cells.size <= 0) {
      return null // Error!
    }

    this.cells.minBy(_.mass)
  }

  // Override

  override def updateSightRange() {
    // For view distance
    var range = 1000d // Base sight range

    if(this.cells.nonEmpty) {
      range += this.cells.head.getSize * 2.5
    }

    this.sightRangeX = range.toInt
    this.sightRangeY = range.toInt
  }

  override def update() {
    // Overrides the update function from player tracker
    // Remove nodes from visible nodes if possible
    this.nodeDestroyQueue.foreach(this.visibleNodes.remove)

    // Update every 500 ms
    if((this.tickViewBox <= 0) && this.gameServer.run) {
      this.visibleNodes = this.calcViewBox()
      this.tickViewBox = 10
    } else {
      this.tickViewBox -= 1
      return
    }

    // Respawn if bot is dead
    if(this.cells.size <= 0) {
      this.gameServer.gameMode.onPlayerSpawn(this.gameServer, this)
      if(this.cells.size == 0) {
        // If the bot cannot spawn any cells, then disconnect it
        this.socket.close()
        return
      }
    }

    // Calc predators/prey
    val cell = this.getLowestCell
    val r = cell.getSize
    this.clearLists()

    // Ignores targeting cells below this mass
    var ignoreMass = Math.min(cell.mass / 10.0, 150)

    // Loop
    this.visibleNodes.foreach(check => Breaks.breakable {
      // Cannot target itself
      if((null == check) || (cell.owner == check.owner)) {
        Breaks.break() // continue
      }

      check.getType match {
        case CellTypes.Player =>
          // Cannot target teammates
          if(this.gameServer.gameMode.haveTeams) {
            if(check.owner.exists(_.team == this.team)) {
              Breaks.break() // continue
            }
          }

          // Check for danger
          if(cell.mass > (check.mass * 1.25)) {
            // Add to prey list
            this.prey += check
          } else if(check.mass > (cell.mass * 1.25)) {
            // Predator
            val dist = this.getDist(cell, check) - (r + check.getSize)
            if(dist < 300) {
              this.predators += check
              if((this.cells.size == 1) && (dist < 0)) {
                this.juke = true
              }
            }
            this.threats += check
          } else {
            this.threats += check
          }

        case CellTypes.Food =>
          this.food += check

        case CellTypes.Virus => // Virus
          this.virus += check

        case CellTypes.EjectedMass => // Ejected mass
          if(cell.mass > 20) {
            this.food += check
          }

        case _ =>
      }
    })

    // Get gamestate
    val newState = this.getState(cell)
    if((newState != this.gameState) && (newState != 4)) {
      // Clear target
      this.target = null
    }
    this.gameState = newState

    // Action
    this.decide(cell)

    this.nodeDestroyQueue = new mutable.HashSet[Cell] // Empty

  }

  // Custom

  def clearLists() {
    this.predators = new ArrayBuffer[Cell]()
    this.threats = new ArrayBuffer[Cell]()
    this.prey = new ArrayBuffer[Cell]()
    this.food = new ArrayBuffer[Cell]()
    this.virus = new ArrayBuffer[Cell]()
    this.juke = false
  }

  def getState(cell : Cell) : Int = {
    // Continue to shoot viruses
    if(this.gameState == 4) {
      return 4
    }

    // Check for predators
    if(this.predators.length <= 0) {
      if(this.prey.length > 0) {
        return 3
      } else if(this.food.length > 0) {
        return 1
      }
    } else if(this.threats.length > 0) {
      if((this.cells.size == 1) && (cell.mass > 180)) {
        val t = this.getBiggest(this.threats)
        val tl = this.findNearbyVirus(t, 500, this.virus)
        if(tl != null) {
          this.target = t
          this.targetVirus = tl
          return 4
        }
      } else {
        // Run
        return 2
      }
    }

    // Bot wanders by default
    0
  }

  def decide(cell : PlayerCell) {
    // The bot decides what to do based on gamestate
    this.gameState match {
      case 0 => // Wander
        //console.log("[Bot] "+cell.getName()+": Wandering");
        if((this.centerPos.x == this.mouse.x) && (this.centerPos.y == this.mouse.y)) {
          // Get a new position
          val index = Math.floor(Math.random() * this.gameServer.nodes.length).toInt
          val randomNode = this.gameServer.nodes(index)

          val pos = if((randomNode.getType == CellTypes.EjectedMass) || (randomNode.getType == CellTypes.Food)) {
            randomNode.position
          } else {
            // Not a food/ejected cell
            this.gameServer.getRandomPosition
          }

          // Set bot's mouse coords to this location
          this.mouse = pos
        }

      case 1 => // Looking for food
        //console.log("[Bot] "+cell.getName()+": Getting Food");
        if((null == this.target) || (!this.visibleNodes.contains(this.target))) {
          // Food is eaten/out of sight... so find a new food cell to target
          this.target = this.findNearest(cell, this.food)

          this.mouse = this.target.position
        }

      case 2 => // Run from (potential) predators
        val avoid = this.combineVectors(this.predators)
        //console.log("[Bot] "+cell.getName()+": Fleeing from "+avoid.getName());

        // Find angle of vector between cell and predator
        val deltaY = avoid.y - cell.position.y
        val deltaX = avoid.x - cell.position.x
        var angle = Math.atan2(deltaX, deltaY)

        // Now reverse the angle
        if(angle > Math.PI) {
          angle -= Math.PI
        } else {
          angle += Math.PI
        }

        // Direction to move
        val x1 = cell.position.x + (500 * Math.sin(angle))
        val y1 = cell.position.y + (500 * Math.cos(angle))

        this.mouse = Position.toInt(x = x1, y = y1)

        // Cheating
        if(cell.mass < 250) {
          cell.mass += 1
        }

        if(this.juke) {
          // Juking
          this.gameServer.splitCells(this)
        }

      case 3 => Breaks.breakable {
        // Target prey
        if((null == this.target) || (cell.mass < (this.target.mass * 1.25)) || (!this.visibleNodes.contains(this.target))) {
          this.target = this.getRandom(this.prey)
        }
        //console.log("[Bot] "+cell.getName()+": Targeting "+this.target.getName());

        this.mouse = this.target.position

        val massReq = 1.25 * (this.target.mass * 2) // Mass required to splitkill the target

        if((cell.mass > massReq) && (this.cells.size == 1)) {
          // Will not split into more than 2 cells
          val splitDist = (4 * (cell.getSpeed * 5)) + (cell.getSize * 1.75)
          // Distance needed to splitkill
          val distToTarget = this.getAccDist(cell, this.target) // Distance between the target and this cell

          if(splitDist >= distToTarget) {
            if((this.threats.length > 0) && (this.getBiggest(this.threats).mass > (1.25 * (cell.mass / 2)))) {
              // Dont splitkill when they are cells that can possibly eat you after the split
              Breaks.break()
            }
            // Splitkill
            this.gameServer.splitCells(this)
          }
        }
      }
      case 4 => Breaks.breakable {
        // Shoot virus

        if((null == this.target) || (null == this.targetVirus) || (this.cells.size != 1) || (!this.visibleNodes.contains(this.target)) || (!this.visibleNodes.contains(this.targetVirus))) {
          this.gameState = 0 // Reset
          this.target = null
          Breaks.break()
        }

        // Make sure target is within range
        val dist = this.getDist(this.targetVirus, this.target) - (this.target.getSize + 100)
        if(dist > 500) {
          this.gameState = 0 // Reset
          this.target = null
          Breaks.break()
        }

        // Find angle of vector between target and virus
        val angle = this.getAngle(this.target, this.targetVirus)

        // Now reverse the angle
        val reversed = this.reverseAngle(angle)

        // Get this bot cell's angle
        val ourAngle = this.getAngle(cell, this.targetVirus)

        // Check if bot cell is in position
        if((ourAngle <= (reversed + .25)) && (ourAngle >= (reversed - .25))) {
          // In position!
          this.mouse = this.targetVirus.position

          // Shoot
          (1 to 7).foreach(_ => {
            this.gameServer.ejectMass(this)
          })

          // Back to starting pos
          this.mouse = cell.position

          // Cleanup
          this.gameState = 0 // Reset
          this.target = null
        } else {
          // Move to position
          val r = cell.getSize
          val x1 = this.targetVirus.position.x + (350 + r) * Math.sin(reversed)
          val y1 = this.targetVirus.position.y + (350 + r) * Math.cos(reversed)
          this.mouse = Position.toInt(x = x1, y = y1)
        }
        // console.log("[Bot] "+cell.getName()+": Targeting (virus) "+this.target.getName());
      }
      case _ =>
        //console.log("[Bot] "+cell.getName()+": Idle "+this.gameState);
        this.gameState = 0

    }

    // Recombining
    if(this.cells.size > 1) {
      val r = this.cells.count(_.recombineTicks == 0)
      // Get amount of cells that can merge

      // Merge
      if(r >= 2) {
        this.mouse = this.centerPos
      }
    }
  }

  // Finds the nearest cell in list
  def findNearest(cell : Cell, list : ArrayBuffer[Cell]) : Cell = {
    /* UNPORTED
        if(this.currentTarget) {
            // Do not check for food if target already exists
            return null
        }
        */

    if(list.isEmpty) {
      return null
    }

    // Check for nearest cell in list
    list.minBy(this.getDist(cell, _))
  }

  def getRandom(list : ArrayBuffer[Cell]) = {
    // Gets a random cell from the array
    val n = Math.floor(Math.random() * list.size).toInt
    list(n)
  }

  def combineVectors(list : mutable.Traversable[Cell]) = {
    // Gets the angles of all enemies approaching the cell
    var x = 0
    var y = 0

    list.foreach(check => {
      x += check.position.x
      y += check.position.y
    })

    // Get avg
    Position.toInt(x = x.toDouble / list.size, y = y.toDouble / list.size)
  }

  def checkPath(cell : Cell, check : Cell) = {
    // Checks if the cell is in the way

    // Get angle of vector (cell -> path)
    val v1 = Math.atan2(cell.position.x - this.mouse.x, cell.position.y - this.mouse.y)

    // Get angle of vector (virus -> cell)
    var v2 = this.getAngle(check, cell)
    v2 = this.reverseAngle(v2)

    (v1 <= (v2 + .25)) && (v1 >= (v2 - .25))
  }

  def getBiggest(list : mutable.Traversable[Cell]) = {
    // Gets the biggest cell from the array
    if(list.isEmpty) null else list.maxBy(_.mass)
  }

  def findNearbyVirus(cell : Cell, checkDist : Double, list : mutable.Traversable[Cell]) : Cell = {
    val r = cell.getSize + 100 // Gets radius + virus radius
    list.foreach(check => {
      val dist = this.getDist(cell, check) - r
      if(checkDist > dist) {
        return check
      }
    })
    null // Returns a bool if no nearby viruses are found
  }

  /* UNPORTED
    def checkPath(cell : Cell, check : Cell) {
        // Get angle of path
        var v1 = Math.atan2(cell.position.x - player.mouse.x, cell.position.y - player.mouse.y);

        // Get angle of vector (cell -> virus)
        var v2 = this.getAngle(cell, check);
        var dist = this.getDist(cell, check);

        var inRange = Math.atan((2 * cell.getSize) / dist); // Opposite/adjacent
        console.log(inRange);
        if((v1 <= (v2 + inRange)) && (v1 >= (v2 - inRange))) {
            // Path collides
            return true;
        }

        // No collide
        return false;
    }
    */

  final def getDist(cell : Cell, check : Cell) = {
    // Fastest distance - I have a crappy computer to test with :(
    var xd = check.position.x - cell.position.x
    xd = if(xd < 0) xd * -1 else xd // Math.abs is slow

    var yd = check.position.y - cell.position.y
    yd = if(yd < 0) yd * -1 else yd // Math.abs is slow

    xd + yd
  }

  final def getAccDist(cell : Cell, check : Cell) = {
    // Accurate Distance
    var xs = check.position.x - cell.position.x
    xs = xs * xs

    var ys = check.position.y - cell.position.y
    ys = ys * ys

    Math.sqrt(xs + ys)
  }

  final def getAngle(c1 : Cell, c2 : Cell) = {
    val deltaY = c1.position.y - c2.position.y
    val deltaX = c1.position.x - c2.position.x
    Math.atan2(deltaX, deltaY)
  }

  final def reverseAngle(angle : Double) = {
    if(angle > Math.PI) {
      angle - Math.PI
    } else {
      angle + Math.PI
    }
  }
}