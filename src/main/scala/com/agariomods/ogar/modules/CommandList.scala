package com.agariomods.ogar.modules

import java.io.PrintWriter

import com.agariomods.ogar._
import com.agariomods.ogar.ai.BotPlayer
import jline.console.ConsoleReader

import scala.collection.mutable.ArrayBuffer
import scala.compat.Platform
import scala.language.implicitConversions
import scala.util.{Success, Failure}

case class CommandList(startTime : Long = Platform.currentTime) {
  implicit def consoleToWriter(console : ConsoleReader) : PrintWriter = new PrintWriter(console.getOutput)

  def loadConfig(gameServer : GameServer, out : PrintWriter): Unit = {
    val fileName = "./gameserver.ini"
    gameServer.loadConfig(fileName) match {
      case Failure(_) =>
        // No config

        // Create a new config
        /* UNPORTED
        out.println("[Game] Config not found... Generating new config")
        fs.writeFileSync('./gameserver.ini', ini.stringify(this.config));
        */
        out.println("[Game] gameserver.ini not found")

      case Success(Left(error)) =>
        out.println(error)
      case Success(Right(cfg)) =>
        // Replace default config's values with the loaded config's values
        cfg.foreach({
          case (_, Left(error)) => out.println(s"[Config] Error: $error")
          case (key, Right(Right(value))) => out.println(s"[Config] $key: $value")
          case (key, Right(Left(value))) => out.println(s"[Config] $key not found in file, current: $value")
        })
    }
  }


  val list : Map[String, (GameServer, Array[String], ConsoleReader) => Unit] = Map(
    "help" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      console.println("[Console] ======================== HELP ======================")
      console.println("[Console] addbot     : add bot to the server")
      console.println("[Console] board      : set scoreboard text")
      console.println("[Console] boardreset : reset scoreboard text")
      console.println("[Console] change     : change specified settings")
      console.println("[Console] clear      : clear console output")
      console.println("[Console] color      : set cell(s) color by client ID")
      console.println("[Console] exit       : stop the server")
      console.println("[Console] food       : spawn food at specified Location")
      console.println("[Console] gamemode   : change server gamemode")
      console.println("[Console] kick       : kick player or bot by client ID")
      console.println("[Console] kill       : kill cell(s) by client ID")
      console.println("[Console] killall    : kill everyone")
      console.println("[Console] mass       : set cell(s) mass by client ID")
      console.println("[Console] name       : change cell(s) name by client ID")
      console.println("[Console] playerlist : get list of players and bots")
      console.println("[Console] pause      : pause game , freeze all cells")
      console.println("[Console] reload     : reload config")
      console.println("[Console] status     : get server status")
      console.println("[Console] tp         : teleport player to specified location")
      console.println("[Console] virus      : spawn virus at a specified Location")
      console.println("[Console] ====================================================")
    }),
    "addbot" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      // Adds 1 bot if user doesnt specify a number
      val add = split.lift(1).flatMap(safeToInt).getOrElse(1)
      (1 to add).foreach(_ => {
        gameServer.bots.addBot()
      })
      console.println("[Console] Added " + add + " player bots")
    }),
    "board" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      val newLB = split.drop(1).to[ArrayBuffer]
      gameServer.fixedLeaderboard = Some(new TextLeaderboard(newLB))
      console.println("[Console] Successfully changed leaderboard values")
    }),
    "boardreset" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      gameServer.fixedLeaderboard = None
      console.println("[Console] Successfully reset leaderboard")
    }),
    "change" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      val kv = split.lift(1).flatMap(key => split.lift(2).map((key, _)))
      kv.foreach({
        case (key, value) =>
          IniReflect.setFromKeyValue(gameServer.config, key, value, "Console") match {
            case Left(error) => console.println(s"[Console] $error")
            case Right(v) => console.println(s"[Console] Set $key to $v")
          }
      })
    }),
    "clear" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      console.clearScreen()
      ()
    }),
    "color" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          val r = split.lift(2).flatMap(safeToInt).map(v => Math.max(Math.min(v, 255), 0))
          val g = split.lift(3).flatMap(safeToInt).map(v => Math.max(Math.min(v, 255), 0))
          val b = split.lift(4).flatMap(safeToInt).map(v => Math.max(Math.min(v, 255), 0))
          val color = new Color(r = r.getOrElse(0), g = g.getOrElse(0), b = b.getOrElse(0))
          gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
            client.setColor(color)
            client.cells.foreach(_.setColor(color))
          })
      }
    }),
    "exit" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      /* UNPORTED
      console.println("[Console] Closing server...")
      gameServer.socketServer.close()
      process.exit(1)
      */
    }),
    "food" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      val kv = split.lift(1).flatMap(safeToInt).flatMap(key => split.lift(2).flatMap(safeToInt).map((key, _)))
      kv match {
        case None =>
          console.println("[Console] Invalid coordinates")
        case Some((x, y)) =>
          val pos = new Position(x = x, y = y)
          val mass = split.lift(3).flatMap(safeToDouble)
          gameServer.spawnFood(pos, mass)
          console.println("[Console] Spawned 1 food cell at (" + pos.x + " , " + pos.y + ")")
      }
    }),
    "gamemode" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a game mode ID!")
        case Some(n) =>
          val gm = gamemodes.get(n) // If there is an invalid gamemode, the function will exit
          gameServer.gameMode.onChange(gameServer) // Reverts the changes of the old gamemode
          gameServer.gameMode = gm // Apply new gamemode
          gameServer.gameMode.onServerInit(gameServer) // Resets the server
          console.println("[Game] Changed game mode to " + gameServer.gameMode.name)
      }
    }),
    "kick" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
            while (client.cells.nonEmpty) {
              gameServer.removeNode(client.cells.head)
            }
            client.socket.close()
            console.println(s"[Console] Kicked ${client.name}")
          })
      }
    }),
    "kill" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
            var count = 0
            while (client.cells.nonEmpty) {
              gameServer.removeNode(client.cells.head)
              count += 1
            }
            console.println(s"[Console] Removed $count cells")
          })
      }
    }),
    "killall" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      var count = 0
      while (gameServer.nodesPlayer.nonEmpty) {
        gameServer.removeNode(gameServer.nodesPlayer.head)
        count += 1
      }
      console.println(s"[Console] Removed $count cells")
    }),
    "mass" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          split.lift(2).flatMap(safeToDouble) match {
            case None =>
              console.println("[Console] Please specify a valid number")
            case Some(amount) =>
              gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
                client.cells.foreach(_.mass = amount)
                console.println(s"[Console] Set mass of ${client.name} to amount")
              })
          }
      }
    }),
    "name" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          val name = split.drop(2).mkString(" ") match {
            case "" => null;
            case s => s
          }
          gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
            console.println(s"[Console] Changing ${client.name} to $name")
            client.name = name
          })
      }
    }),
    "playerlist" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      console.println("[Console] Showing " + gameServer.clients.length + " players: ")
      console.println(" ID         | IP              | " + fillChar("NICK", ' ', gameServer.config.playerMaxNickLength) + " | CELLS | SCORE  | POSITION    ") // Fill space
      console.println(fillChar("", '-', " ID         | IP              |  | CELLS | SCORE  | POSITION    ".length + gameServer.config.playerMaxNickLength))
      gameServer.clients.view.map(_.playerTracker).foreach(client => {
        // ID with 3 digits length
        val id = fillChar(client.pID, ' ', 10, rTL = true)
        // Get ip (15 digits length)
        val ip = fillChar(client.socket.remoteAddress, ' ', 15)
        // Get name and data
        var nick = ""
        var cells = ""
        var score = ""
        var position = ""
        var data = ""
        def emptyToUnnamed(s : String) = if(s == "") "An unnamed cell" else s
        if(client.spectate) {
          nick = emptyToUnnamed(client.getSpectatedPlayer.map(_.name).getOrElse(""))
          data = fillChar(s"SPECTATING: $nick", '-', " | CELLS | SCORE | POSITION    ".length + gameServer.config.playerMaxNickLength, rTL = true)
          console.println(" " + id + " | " + ip + " | " + data)
        } else if(client.cells.size > 0) {
          nick = fillChar(emptyToUnnamed(client.name), ' ', gameServer.config.playerMaxNickLength)
          cells = fillChar(client.cells.size, ' ', 5, rTL = true)
          score = fillChar(client.getScore(recalculate = true), ' ', 6, rTL = true)
          position = fillChar(client.centerPos.x, ' ', 5, rTL = true) + ", " + fillChar(client.centerPos.y, ' ', 5, rTL = true)
          console.println(" " + id + " | " + ip + " | " + nick + " | " + cells + " | " + score + " | " + position)
        } else {
          // No cells = dead player or in-menu
          data = fillChar("DEAD OR NOT PLAYING", '-', " | CELLS | SCORE | POSITION    ".length + gameServer.config.playerMaxNickLength, rTL = true)
          console.println(" " + id + " | " + ip + " | " + data)
        }
      })
    }),
    "pause" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      gameServer.run = !gameServer.run
      // Switches the pause state
      val s = if(gameServer.run) "Unpaused" else "Paused"
      console.println(s"[Console] $s the game.")
    }),
    "reload" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      loadConfig(gameServer, consoleToWriter(console))
      console.println("[Console] Reloaded the config file successfully")
    }),
    "status" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      // Get amount of humans/bots
      var humans = 0
      var bots = 0
      gameServer.clients.view.map(_.playerTracker).foreach({
        case _ : BotPlayer => bots += 1
        case _ : PlayerTracker => humans += 1
      })
      val runtime = Runtime.getRuntime
      val total = runtime.totalMemory()
      val free = runtime.freeMemory()
      console.println(s"[Console] Connected players: ${gameServer.clients.length}/${gameServer.config.serverMaxConnections}")
      console.println(s"[Console] Players: $humans Bots: $bots")
      console.println(s"[Console] Server has been running for ${(Platform.currentTime - startTime) / 1000} seconds.")
      console.println(s"[Console] Current memory usage: ${(total - free) / 1000}/${total / 1000} kb")
      console.println(s"[Console] Current game mode: ${gameServer.gameMode.name}")
    }),
    "tp" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      split.lift(1).flatMap(safeToInt) match {
        case None =>
          console.println("[Console] Please specify a valid player ID!")
        case Some(id) =>
          val kv = split.lift(2).flatMap(safeToInt).flatMap(key => split.lift(3).flatMap(safeToInt).map((key, _)))
          kv match {
            case None =>
              console.println("[Console] Invalid coordinates")
            case Some((x, y)) =>
              val pos = new Position(x = x, y = y)
              gameServer.clients.view.map(_.playerTracker).find(_.pID == id).fold(console.println(s"[Console] Client with such id not found"))(client => {
                client.cells.foreach(_.position = pos)
                console.println(s"[Console] Teleported ${client.name} to (${pos.x} , ${pos.y})")
              })
          }
      }
    }),
    "virus" -> ((gameServer : GameServer, split : Array[String], console : ConsoleReader) => {
      val kv = split.lift(1).flatMap(safeToInt).flatMap(key => split.lift(2).flatMap(safeToInt).map((key, _)))
      kv match {
        case None =>
          console.println("[Console] Invalid coordinates")
        case Some((x, y)) =>
          val pos = new Position(x = x, y = y)
          val mass = split.lift(3).flatMap(safeToDouble)
          gameServer.spawnVirus(pos, mass)
          console.println("[Console] Spawned 1 virus at (" + pos.x + " , " + pos.y + ")")
      }
    })
  )

  // Utils
  def fillChar(data : Any, c : Char, fieldLength : Int, rTL : Boolean = false) = {
    var result = data.toString
    if(rTL) {
      while (result.length < fieldLength)
        result = s"$c$result"
    } else {
      while (result.length < fieldLength)
        result = s"$result$c"
    }
    result
  }

  def safeToInt(s : String) : Option[Int] = try Some(java.lang.Integer.parseInt(s)) catch {
    case _ : NumberFormatException => None
  }

  def safeToDouble(s : String) : Option[Double] = try Some(java.lang.Double.parseDouble(s)) catch {
    case _ : NumberFormatException => None
  }
}
