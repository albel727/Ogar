package com.agariomods.ogar

final case class ServerStats(
                              current_players : Int = 0,
                              alive : Int = 0,
                              spectators : Int = 0,
                              max_players : Int = 0,
                              gamemode : String = "",
                              start_time : Long = 0
                              )
