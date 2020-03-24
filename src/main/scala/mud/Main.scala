package mud

import scala.io.StdIn._
import akka.actor.Props
import akka.actor.ActorSystem
import scala.concurrent.duration._
import java.io.PrintStream
import java.io.BufferedReader
import java.io.OutputStream
import java.io.InputStream
import java.io.InputStreamReader
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main {

	val system = ActorSystem("Main")
	val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
	val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
	system.scheduler.schedule(1.seconds, 0.1.seconds, playerManager, PlayerManager.CheckAllInputs)

	def main(args: Array[String]): Unit = {

		println("Welcome to my MUD. What is your name?")
		val playerName = readLine().trim()
		println(s"Hello $playerName.")

		var playerRoom:String = "-1"
		while(playerRoom == "-1"){
			println("Select the world you want to enter.")
			println("1. Dojo\n2. Underground \n3. Nanfang Village")
			readLine().trim match {
				case "1" => playerRoom = "Dojo"
				case "2" => playerRoom = "Underground"
				case "3" => playerRoom = "Village"
				case _ => println("Please enter a valid number.")
			}
		}

		playerManager ! PlayerManager.CreatePlayer(playerName, playerRoom, Console.in, Console.out)
	}
}
