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

		println(s"Do you want your player to be called $name (Y/N)?")
		readline().toLowerCase match{
			case y => val playerName:String = name
			case n =>
			println("What is your player's name?")
			val playerName:String = readline()
		}

		//println("Enter the password for your player.")

		println("Select ")
	}
}
