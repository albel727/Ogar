package com.agariomods.ogar.packet

import java.nio.ByteOrder

import akka.util.ByteStringBuilder
import com.agariomods.ogar.entity.Cell

class UpdateNodes(
                   destroyQueue : collection.mutable.HashSet[Cell],
                   nodes : collection.mutable.ArrayBuffer[Cell],
                   nonVisibleNodes : collection.mutable.ArrayBuffer[Cell]
                   ) extends Packet {

  def build() = {
    implicit val order = ByteOrder.LITTLE_ENDIAN

    // Calculate nodes sub packet size before making the data view
    val view = new ByteStringBuilder

    view.putByte(16) // Packet ID
    view.putShort(this.destroyQueue.size) // Nodes to be destroyed

    this.destroyQueue.foreach(node => {
      if(null != node) {
        val killer = node.getKiller.map(_.nodeId).getOrElse(0)

        view.putInt(killer) // Killer ID
        view.putInt(node.nodeId) // Node ID
      }
    })

    this.nodes.foreach(node => {
      if(null != node) {
        view.putInt(node.nodeId) // Node ID
        view.putInt(node.position.x) // X position
        view.putInt(node.position.y) // Y position
        view.putShort(node.getSize) // Mass formula: Radius (size) = (mass * mass) / 100
        view.putByte(node.color.r.toByte) // Color (R)
        view.putByte(node.color.g.toByte) // Color (G)
        view.putByte(node.color.b.toByte) // Color (B)
        view.putByte(node.spiked) // Flags

        val name = node.getName
        if(null != name) {
          name.foreach(view.putShort(_))
        }

        view.putShort(0) // End of string
      }
    })

    val len = this.nonVisibleNodes.length + this.destroyQueue.size
    view.putInt(0) // End
    view.putInt(len) // # of non-visible nodes to destroy

    // Destroy queue + nonvisible nodes
    this.destroyQueue.foreach(node => {
      if(null != node) {
        view.putInt(node.nodeId)
      }
    })

    this.nonVisibleNodes.foreach(node => {
      if(null != node) {
        view.putInt(node.nodeId)
      }
    })

    view.result()
  }
}