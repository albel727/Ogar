package com.agariomods.ogar

class GameConfig( // Border - Right: X increases, Down: Y increases (as of 2015-05-20)
                  var serverMaxConnections : Int = 64, // Maximum amount of connections to the server.
                  var serverInterface : String = "0.0.0.0", // Server listen interface
                  var serverPort : Int = 443, // Server port
                  var serverGamemode : Int = 0, // Gamemode, 0 = FFA, 1 = Teams
                  var serverBots : Int = 0, // Amount of player bots to spawn
                  var serverViewBaseX : Int = 1024, // Base view distance of players. Warning: high values may cause lag
                  var serverViewBaseY : Int = 592,
                  var serverStatsPort : Int = 88, // Port for stats server. Having a negative number will disable the stats server.
                  var serverStatsUpdate : Int = 60, // Amount of seconds per update for the server stats
                  var serverLogLevel : Int = 1, // Logging level of the server. 0 = No logs, 1 = Logs the console, 2 = Logs console and ip connections
                  var borderLeft : Int = 0, // Left border of map (Vanilla value: 0)
                  var borderRight : Int = 6000, // Right border of map (Vanilla value: 11180.3398875)
                  var borderTop : Int = 0, // Top border of map (Vanilla value: 0)
                  var borderBottom : Int = 6000, // Bottom border of map (Vanilla value: 11180.3398875)
                  var spawnInterval : Int = 20, // The interval between each food cell spawn in ticks (1 tick = 50 ms)
                  var foodSpawnAmount : Int = 10, // The amount of food to spawn per interval
                  var foodStartAmount : Int = 100, // The starting amount of food in the map
                  var foodMaxAmount : Int = 500, // Maximum food cells on the map
                  var foodMass : Int = 1, // Starting food size (In mass)
                  var virusMinAmount : Int = 10, // Minimum amount of viruses on the map.
                  var virusMaxAmount : Int = 50, // Maximum amount of viruses on the map. If this amount is reached, then ejected cells will pass through viruses.
                  var virusStartMass : Double = 100, // Starting virus size (In mass)
                  var virusFeedAmount : Int = 7, // Amount of times you need to feed a virus to shoot it
                  var ejectMass : Int = 12, // Mass of ejected cells
                  var ejectMassLoss : Int = 16, // Mass lost when ejecting cells
                  var ejectSpeed : Int = 160, // Base speed of ejected cells
                  var ejectSpawnPlayer : Int = 50, // Chance for a player to spawn from ejected mass
                  var playerStartMass : Int = 10, // Starting mass of the player cell.
                  var playerMaxMass : Int = 22500, // Maximum mass a player can have
                  var playerMinMassEject : Int = 32, // Mass required to eject a cell
                  var playerMinMassSplit : Int = 36, // Mass required to split
                  var playerMaxCells : Int = 16, // Max cells the player is allowed to have
                  var playerRecombineTime : Int = 30, // Base amount of seconds before a cell is allowed to recombine
                  var playerMassDecayRate : Double = 0.002, // Amount of mass lost per second
                  var playerMinMassDecay : Int = 9, // Minimum mass for decay to occur
                  var playerMaxNickLength : Int = 15, // Maximum nick length
                  var playerDisconnectTime : Int = 60, // The amount of seconds it takes for a player cell to be removed after disconnection (If set to -1, cells are never removed)
                  var tourneyMaxPlayers : Int = 12, // Maximum amount of participants for tournament style game modes
                  var tourneyPrepTime : Int = 10, // Amount of ticks to wait after all players are ready (1 tick = 1000 ms)
                  var tourneyEndTime : Int = 30, // Amount of ticks to wait after a player wins (1 tick = 1000 ms)
                  var tourneyTimeLimit : Int = 20, // Time limit of the game, in minutes.
                  var tourneyAutoFill : Int = 0, // If set to a value higher than 0, the tournament match will automatically fill up with bots after this amount of seconds
                  var tourneyAutoFillPlayers : Int = 1 // The timer for filling the server with bots will not count down unless there is this amount of real players
                  )
