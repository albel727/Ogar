package com.agariomods.ogar.ai

import com.agariomods.ogar.GameServer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class BotLoader(val gameServer : GameServer) {
  var randomNames = new ArrayBuffer[String]
  var nameIndex = 0

  this.loadNames()

  def getName = {
    var name = ""

    // Picks a random name for the bot
    if(this.randomNames.length > 0) {
      val index = Math.floor(Math.random() * this.randomNames.length).toInt
      name = this.randomNames(index)
      this.randomNames.remove(index)
    } else {
      this.nameIndex += 1
      name = s"bot${this.nameIndex}"
    }

    name
  }

  def loadNames() {
    // Load names
    try {
      // Read and parse the names - filter out whitespace-only names
      this.randomNames = Source.fromFile("./botnames.txt", "UTF-8").getLines().map(_.trim).filter(_.nonEmpty).to[ArrayBuffer]
    } catch {
      case e : Exception =>
      // Nothing, use the default names
    }

    this.nameIndex = 0
  }

  def addBot() {
    val s = new FakeSocket(this.gameServer)

    // Add to world
    s.packetHandler.setNickname(this.getName)
  }
}