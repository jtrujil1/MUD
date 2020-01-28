package mud

import scala.io.StdIn._
/**
This is a stub for the main class for your MUD.
*/
object Main {
	def main(args: Array[String]): Unit = {
		println("Welcome to my MUD. What is your name?")
		val name = readLine().trim()
		println(s"Hello $name.")

		var playerName:String = ""
		println(s"Do you want your player to be called $name (Y/N)?")
		def response:Unit = 
		readLine().toLowerCase match{
			case "y" | "yes" => playerName = name
			case "n" | "no" =>
			println("What is your player's name?")
			playerName = readLine()
		}

		//println("Enter the password for your player.")
		var playerRoom: Int = 0
		println("Select the world you want to enter.")
		println("1. Dojo\n2. Underground \n3. Nanfang Village")
		readInt() match {
			case 1 => playerRoom = 0
			case 2 => playerRoom = 1
			case 3 => playerRoom = 2
			case _ => println("Please enter a valid number.")
		}

		Room.readRooms()

		val player = new Player(playerName, Nil, Room.room(playerRoom))

		var command = readLine()
		while (command != "exit"){
				var commandArray = command.toLowerCase.split(" +", 2)
				if(commandArray(0).trim == "get")
					player.processCommand("get", commandArray(1).trim)
				if (commandArray(0).trim == "drop")
					player.processCommand("drop", commandArray(1).trim)
				else
					player.processCommand(command)
		}
	}
}
