package com.agariomods.ogar

import com.agariomods.ogar.entity.{Cell, PlayerCell, PlayerTrackerWithBrainCells, PlayerTrackerWithHeroCells}
import com.agariomods.ogar.net.OgarSocket
import com.agariomods.ogar.packet.{UpdateNodes, UpdatePosition}

import scala.collection.mutable
import scala.util.control.Breaks._

class PlayerTracker(val gameServer : GameServer, val socket : OgarSocket)
  extends PlayerTrackerWithHeroCells
  with PlayerTrackerWithBrainCells
{
  var pID = -1
  var disconnect = -1 // Disconnection

  var name = ""

  var nodeAdditionQueue = new mutable.HashSet[Cell]
  var nodeDestroyQueue = new mutable.HashSet[Cell]
  var visibleNodes = new mutable.HashSet[Cell]
  val cells = new mutable.HashSet[PlayerCell]
  var score = 0d // Needed for leaderboard

  var mouse = Position.Zero
  val mouseCells = mutable.HashMap.empty[Int, Position] // For individual cell movement

  var tickLeaderboard = 0 //
  var tickViewBox = 0

  var team = 0
  var spectate = false
  var spectatedPlayer = -1 // Current player that this player is watching

  // Viewing box
  var sightRangeX = 0
  var sightRangeY = 0
  var centerPos = new Position(3000, 3000)
  var viewBox = new Box(0, 0, 0, 0)

  var color = new Color(255, 255, 255)

  // Gamemode function
  if (null != gameServer) {
    this.pID = gameServer.getNewPlayerID
    gameServer.gameMode.onPlayerInit(this)
  }

  // Setters/Getters

  def setName(name : String) {
    this.name = name
  }

  def getName = name

  def getScore(recalculate : Boolean) = {
    if (recalculate) {
      this.score = this.cells.map(_.mass).sum
    }
    this.score.floor.toInt
  }

  def setColor(color : Color) {
    this.color = color
  }

  def getTeam = team

  // Functions

  def update() {
    // Actions buffer (So that people cant spam packets)
    if (this.socket.packetHandler.pressSpace) { // Split cell
      this.gameServer.gameMode.pressSpace(this.gameServer,this)
      this.socket.packetHandler.pressSpace = false
    }

    if (this.socket.packetHandler.pressW) { // Eject mass
      this.gameServer.gameMode.pressW(this.gameServer,this)
      this.socket.packetHandler.pressW = false
    }

    if (this.socket.packetHandler.pressQ) { // Q Press
      this.gameServer.gameMode.pressQ(this.gameServer,this)
      this.socket.packetHandler.pressQ = false
    }

    val updateNodes = collection.mutable.ArrayBuffer[Cell]() // Nodes that need to be updated via packet

    //var d = 0
    {
      // Remove nodes from visible nodes if possible
      // Filter out nodes that were never visible anyway
      val newDestroyQueue = this.nodeDestroyQueue.filter(this.visibleNodes.remove)
      //d = this.nodeDestroyQueue.size - newDestroyQueue.size
      this.nodeDestroyQueue = newDestroyQueue
    }

    // Get visible nodes every 400 ms
    val nonVisibleNodes = collection.mutable.ArrayBuffer[Cell]() // Nodes that are not visible
    if (this.tickViewBox <= 0) {
      val newVisible = this.calcViewBox()

      // Compare and destroy nodes that are not seen
      this.visibleNodes.foreach(visibleNode => {
        if(!newVisible.contains(visibleNode)) {
          // Not seen by the client anymore
          nonVisibleNodes += visibleNode
        }
      })

      // Add nodes to client's screen if client has not seen it already
      newVisible.foreach(newVisibleNode => {
        if(!visibleNodes.contains(newVisibleNode)) {
          updateNodes += newVisibleNode
        }
      })

      this.visibleNodes = newVisible
      // Reset Ticks
      this.tickViewBox = 2
    } else {
      this.tickViewBox -= 1
      // Add nodes to screen

      this.nodeAdditionQueue.foreach(node => {
        this.visibleNodes += node
        updateNodes += node
      })
    }

    // Update moving nodes
    this.visibleNodes.foreach(node => {
      if (node.sendUpdate()) {
        // Sends an update if cell is moving
        updateNodes += node
      }
    })

    // Send packet
    this.socket.sendPacket(new UpdateNodes(this.nodeDestroyQueue, updateNodes, nonVisibleNodes))

    this.nodeDestroyQueue = mutable.HashSet.empty // Reset destroy queue
    this.nodeAdditionQueue = mutable.HashSet.empty // Reset addition queue

    // Update leaderboard
    if (this.tickLeaderboard <= 0) {
      this.socket.sendPacket(this.gameServer.lb_packet)
      this.tickLeaderboard = 10 // 20 ticks = 1 second
    } else {
      this.tickLeaderboard -= 1
    }

    // Handles disconnections
    if (this.disconnect > -1) {
      // Player has disconnected... remove it when the timer hits -1
      this.disconnect -= 1
      if (this.disconnect == -1) {
        // Remove all client cells

        this.socket.playerTracker.cells.foreach(cell => {
          breakable {
            if(null == cell) {
              break() //continue
            }

            this.gameServer.removeNode(cell)
          }
        })

        // Remove from client list
        this.gameServer.clients -= this.socket
      }
    }
  }

  // Viewing box

  def updateSightRange() { // For view distance
    var totalSize = 1.0

    this.cells.foreach(cell => {
      breakable {
        if(null == cell) {
          break() //continue
        }

        totalSize += cell.getSize
      }
    })

    val factor = Math.pow(Math.min(64.0 / totalSize, 1), 0.4)
    this.sightRangeX = (this.gameServer.config.serverViewBaseX / factor).toInt
    this.sightRangeY = (this.gameServer.config.serverViewBaseY / factor).toInt
  }

  def updateCenter() { // Get center of cells
    val len = this.cells.size

    if (len <= 0) {
      return // End the function if no cells exist
    }

    var x = 0
    var y = 0
    this.cells.foreach(cell => {
      breakable {
        if(null == cell) {
          break() //continue
        }

        x += cell.position.x
        y += cell.position.y
      }
    })

    this.centerPos = Position.toInt(x = x.toDouble / len, y = y.toDouble / len)
  }

  def calcViewBox() : mutable.HashSet[Cell] = {
    if (this.spectate) {
      // Spectate mode
      return this.getSpectateNodes
    }

    // Main function
    this.updateSightRange()
    this.updateCenter()

    // Box
    this.viewBox.topY = this.centerPos.y - this.sightRangeY
    this.viewBox.bottomY = this.centerPos.y + this.sightRangeY
    this.viewBox.leftX = this.centerPos.x - this.sightRangeX
    this.viewBox.rightX = this.centerPos.x + this.sightRangeX
    //this.viewBox.width = this.sightRangeX
    //this.viewBox.height = this.sightRangeY

    val newVisible = new mutable.HashSet[Cell]
    this.gameServer.nodes.foreach(node => {
      breakable {
        if(null == node) {
          break() //continue
        }

        if(node.visibleCheck(this.viewBox, this.centerPos)) {
          // Cell is in range of viewBox
          newVisible.add(node)
        }
      }
    })

    newVisible
  }


  def getSpectatedPlayer : Option[PlayerTracker] = {
    val specPlayer : PlayerTracker = if(!this.spectate) {
      null
    } else if(this.gameServer.getMode.specByLeaderboard && this.gameServer.leaderboard.isInstanceOf[PlayerLeaderboard]) {
      this.spectatedPlayer = Math.min(this.gameServer.leaderboard.length - 1, this.spectatedPlayer)
      if(this.spectatedPlayer == -1) null else this.gameServer.leaderboard.asInstanceOf[PlayerLeaderboard].leaders(this.spectatedPlayer)
    } else {
      this.spectatedPlayer = Math.min(this.gameServer.clients.length - 1, this.spectatedPlayer)
      if(this.spectatedPlayer == -1) null else this.gameServer.clients(this.spectatedPlayer).playerTracker
    }
    Option(specPlayer)
  }

  def getSpectateNodes : mutable.HashSet[Cell] = {
    getSpectatedPlayer.map(specPlayer => {
      // If selected player has died/disconnected, switch spectator and try again next tick
      if (specPlayer.cells.size == 0) {
        this.gameServer.switchSpectator(this)
        return mutable.HashSet.empty
      }

      // Get spectated player's location and calculate zoom amount
      var specZoom = Math.sqrt(100 * specPlayer.score)
      specZoom = Math.pow(Math.min(40.5 / specZoom, 1.0), 0.4) * 0.6
      // TODO: Send packet elsewhere so it is send more often
      this.socket.sendPacket(new UpdatePosition(specPlayer.centerPos.x, specPlayer.centerPos.y, specZoom))
      // TODO: Recalculate visible nodes for spectator to match specZoom
      val result = specPlayer.visibleNodes.map(identity)
      result
    }).getOrElse(mutable.HashSet.empty) // Nothing
  }
}
