package com.agariomods.ogar.entity

import com.agariomods.ogar.GameServer

trait ExploderCell {
  this : Cell =>

  override def onConsume(consumer : PlayerCell, gameServer : GameServer) {
    val client = consumer.owner.get

    val maxSplits : Int = Math.floor(consumer.mass / 16.0).toInt - 1 // Maximum amount of splits
    var numSplits : Int = gameServer.config.playerMaxCells - client.cells.size // Get number of splits
    numSplits = Math.min(numSplits, maxSplits)
    var splitMass = Math.min(consumer.mass / (numSplits + 1.0), 36) // Maximum size of new splits

    // Cell consumes mass before splitting
    consumer.addMass(this.mass)

    // Cell cannot split any further
    if (numSplits <= 0) {
      return
    }

    // Big cells will split into cells larger than 36 mass (1/4 of their mass)
    var bigSplits = 0
    val endMass = consumer.mass - (numSplits * splitMass)
    if ((endMass > 300) && (numSplits > 0)) {
      bigSplits += 1
      numSplits -= 1
    }
    if ((endMass > 1200) && (numSplits > 0)) {
      bigSplits += 1
      numSplits -= 1
    }
    if ((endMass > 3000) && (numSplits > 0)) {
      bigSplits += 1
      numSplits -= 1
    }

    // Splitting
    var angle = 0d // Starting angle
    var k = 0
    while(k < numSplits) {
      angle += 6.0 / numSplits // Get directions of splitting cells
      gameServer.newCellVirused(client, consumer, angle, splitMass, 150)
      consumer.mass -= splitMass
      k += 1
    }

    k = 0
    while(k < bigSplits) {
      angle = Math.random() * 6.28 // Random directions
      splitMass = consumer.mass / 4.0
      gameServer.newCellVirused(client, consumer, angle, splitMass,20)
      consumer.mass -= splitMass
      k += 1
    }

    // Prevent consumer cell from merging with other cells
    gameServer.gameMode.calcMergeTime(consumer, gameServer.config.playerRecombineTime)
  }
}
