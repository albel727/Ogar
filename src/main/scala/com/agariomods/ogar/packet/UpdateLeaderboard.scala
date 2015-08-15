package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.{ByteString, ByteStringBuilder}
import com.agariomods.ogar.{Leaderboard, PlayerLeaderboard, TeamLeaderboard, TextLeaderboard}

class UpdateLeaderboard(leaderboard : Leaderboard) extends Packet {

  def build() : ByteString = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    leaderboard match {
      case TextLeaderboard(lines) => // Custom Text List
        val view = new ByteStringBuilder

        val tmp = lines.filter(null != _)

        // Packet ID
        view.putByte(49)
        // Number of elements
        view.putInt(tmp.size)

        // Loop through strings
        tmp.foreach(item => {
          view.putInt(1)
          item.foreach(view.putShort(_))
          view.putShort(0)
        })

        view.result()
      case PlayerLeaderboard(leaders) => // FFA-type Packet (List)
        val tmp = leaders.filter(null != _)

        val view = new ByteStringBuilder

        // Packet ID
        view.putByte(49)
        // Number of elements
        view.putInt(tmp.size)

        tmp.foreach(item => {
          // Get node id of player's 1st cell
          val nodeID = item.cells.headOption.map(c => c.nodeId).getOrElse(0)

          view.putInt(nodeID)

          // Set name
          val name = item.getName
          if(null != name) {
            name.foreach(view.putShort(_))
          }

          view.putShort(0)
        })

        view.result()
      case TeamLeaderboard(teams) =>  // Teams-type Packet (Pie Chart)
        val view = new ByteStringBuilder

        val tmp = teams

        // Packet ID
        view.putByte(50)
        // Number of elements
        view.putInt(tmp.size)

        tmp.foreach(view.putFloat)

        view.result()
    }
  }
}