package com.agariomods.ogar.gamemodes

import com.agariomods.ogar.entity.Cell
import com.agariomods.ogar.packet.DrawLine
import com.agariomods.ogar.{GameServer, PlayerTracker}
import org.slf4j.LoggerFactory

object Debug {
  val logger = LoggerFactory.getLogger(classOf[Debug])
}

class Debug extends FFA {
  override val ID = 21
  override val name = "Debug Mode"
  override val specByLeaderboard = false

  // Gamemode Specific Functions

  def testPath(gameServer : GameServer, player : PlayerTracker) {
    val cell = player.cells.head
    val check = gameServer.nodesVirus.head

    val v1 = Math.atan2(cell.position.x - player.mouse.x, cell.position.y - player.mouse.y)

    // Get angle of vector (cell -> virus)
    val v2 = this.getAngle(cell, check)
    val dist = this.getDist(cell, check)

    val inRange = Math.atan((2 * cell.getSize) / dist) // Opposite/adjacent

    val collided = (v1 <= (v2 + inRange)) && (v1 >= (v2 - inRange))

    Debug.logger.info("Angles: {}, {}", v1, v2)
    Debug.logger.info("InRange: {}; Collided: {}", inRange, collided)
  }

  final def getAngle(c1 : Cell, c2 : Cell) = {
    val deltaY = c1.position.y - c2.position.y
    val deltaX = c1.position.x - c2.position.x
    Math.atan2(deltaX, deltaY)
  }

  final def getDist(cell : Cell, check : Cell) = {
    // Fastest distance - I have a crappy computer to test with :(
    var xd = check.position.x - cell.position.x
    xd = if (xd < 0) xd * -1 else xd // Math.abs is slow

    var yd = check.position.y - cell.position.y
    yd = if (yd < 0) yd * -1 else yd // Math.abs is slow

    xd + yd
  }

  // Override

  override def pressW(gameServer : GameServer, player : PlayerTracker) {
    // Called when the Q key is pressed
    println("Test:")
    this.testPath(gameServer, player)
    player.socket.sendPacket(new DrawLine(player.mouse.x, player.mouse.y))
  }
}