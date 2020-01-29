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
		while(playerName == ""){
			println(s"Do you want your player to be called $name (Y/N)?")
			val response = readLine()
			response.toLowerCase.trim match{
				case "y" | "yes" => playerName = name
				case "n" | "no" =>
				println("What is your player's name?")
				playerName = readLine()
				case _ => 
				println("Please enter a valid answer.")
			}
		}

		//println("Enter the password for your player.")
		var playerRoom: Int = -1
		while(playerRoom == -1){
			println("Select the world you want to enter.")
			println("1. Dojo\n2. Underground \n3. Nanfang Village")
			readLine().trim match {
				case "1" => playerRoom = 0
				case "2" => playerRoom = 1
				case "3" => playerRoom = 2
				case _ => println("Please enter a valid number.")
			}
		}

		//Room.readRooms()

		val player1 = new Player(playerName, Nil, Room.rooms(playerRoom))

		var command = ""
		while (command.trim != "exit"){
			println("Enter a command.")
			command = readLine()
			var commandArray = command.toLowerCase.split(" +", 2)
			if(commandArray(0).trim == "get")
				if(commandArray.length <= 1) 
					println("Invalid command, please try again.")
				else 
					player1.processCommand("get", commandArray(1).trim)
			if (commandArray(0).trim == "drop")
				if(commandArray.length <= 1) 
					println("Invalid command, please try again.")
				else 
				player1.processCommand("drop", commandArray(1).trim)
			else
				player1.processCommand(command.toLowerCase.trim)
		}
	}
}
