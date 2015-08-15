package com.agariomods.ogar.entity

sealed trait CellType {
  val typeId : Int
}

object CellTypes {
  object Player extends CellType {
    val typeId = 0
  }

  object Food extends CellType {
    val typeId = 1
  }

  object Virus extends CellType {
    val typeId = 2
  }

  object EjectedMass extends CellType {
    val typeId = 3
  }

  object Hero extends CellType {
    val typeId = 130
  }

  object Brain extends CellType {
    val typeId = 131
  }
}
