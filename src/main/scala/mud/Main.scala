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
		readLine().toLowerCase match{
			case y => playerName = name
			case n =>
			println("What is your player's name?")
			playerName = readLine()
		}

		//println("Enter the password for your player.")
		var playerRoom: Int = 0
		println("Select the world you want to enter.")
		println("1. Room A\n2. Room B \n3. Room C")
		readInt() match {
			case 1 => playerRoom = 0
			case 2 => playerRoom = 1
			case 3 => playerRoom = 2
			case _ => println("Please enter a valid number.")
		}

		val player = new Player(playerName, Nil, ???)

		var command = readLine()
		while (command != "exit"){
				if(command.toLowerCase.substring(0,3) == "get")
					player.processCommand("get", command.toLowerCase.substring(4,command.length))
				if (command.toLowerCase.substring(0,4) == "drop")
					player.processCommand("drop", command.toLowerCase.substring(5,command.length))
				else
					player.processCommand(command)
		}
	}
}
