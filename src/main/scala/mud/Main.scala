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
import java.net.ServerSocket
import scala.concurrent.Future
import java.net.Socket

object Main extends App {

	val system = ActorSystem("Main")
	val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
	val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
	system.scheduler.schedule(1.seconds, 0.1.seconds, playerManager, PlayerManager.CheckAllInputs)

	val port = 8000
	val ss = new ServerSocket(port)
	println(s"Connected to port $port.")
	
	while(true){
		val sock = ss.accept()
    	Future {
			val out = new PrintStream(sock.getOutputStream())
			val in = new BufferedReader(new InputStreamReader(sock.getInputStream()))
			
			out.println("Welcome to my MUD. What is your name?")
			val playerName = in.readLine().trim()
			out.println(s"Hello $playerName.")

			var playerRoom:String = "-1"
			while(playerRoom == "-1"){
				out.println("Select the world you want to enter.")
				out.println("1. Dojo\n2. Underground \n3. Nanfang Village \n4. Red Lake")
				in.readLine().trim match {
					case "1" => playerRoom = "Dojo"
					case "2" => playerRoom = "Underground"
					case "3" => playerRoom = "Village"
					case "4" => playerRoom = "Lake"
					case _ => out.println("Please enter a valid number.")
				}
			}
			playerManager ! PlayerManager.CreatePlayer(playerName, playerRoom, sock, in, out)
		}
	}
}
