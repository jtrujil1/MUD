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
		var playerRoom:String = "-1"
		while(playerRoom == "-1"){
			println("Select the world you want to enter.")
			println("1. Dojo\n2. Underground \n3. Nanfang Village")
			readLine().trim match {
				case "1" => playerRoom = "Dojo"
				case "2" => playerRoom = "Underground"
				case "3" => playerRoom = "Nanfang"
				case _ => println("Please enter a valid number.")
			}
		}

		printf("\nYou have chosen the %s\n", Room.rooms(playerRoom).name)

		val player1 = new Player(Room.rooms(playerRoom), Nil, playerName)

		var command = ""
		while (command.trim != "exit"){
			println("Enter a command.")
			command = readLine()
			player1.processCommand(command)
		}
	}
}
