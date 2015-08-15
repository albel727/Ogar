package com.agariomods.ogar

package object gamemodes {
  def get(id : Int) : Mode = {
    id match {
      case 1 => // Teams
        new Teams()
      case 2 => // Experimental
        new Experimental()
      case 10 => // Tournament
        new Tournament()
      case 11 => // Hunger Games
        new HungerGames()
      case 12 => // Zombie
        new Zombie()
      case 13 => // Zombie Team
        new TeamZ()
      case 14 => // Experimental Team
        new TeamX()
      case 20 => // Rainbow
        new Rainbow()
      case 21 => // Debug
        new Debug()
      case _ => // FFA is default
        new FFA()
    }
  }
}
