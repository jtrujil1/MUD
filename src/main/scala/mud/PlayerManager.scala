package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import java.io.BufferedReader
import java.io.PrintStream
import java.net.Socket
import scala.collection.mutable
import java.{util => ju}

class PlayerManager extends Actor {

    val players = mutable.Map[String, ActorRef]()

    import PlayerManager._
    def receive = {
        case CreatePlayer(playerName, playerRoom, sock, in, out) => 
            val newPlayer = context.actorOf(Props(new Player(playerName, sock, in, out, List())), playerName)
            players += (playerName -> newPlayer)
            Main.roomManager ! RoomManager.AddPlayerToRoom(newPlayer, playerRoom)
        case CheckAllInputs =>
            for (child <- context.children) child ! Player.CheckInput
        case TellPlayer(playerName, receiverName, message) =>
            try{
            val receiver = players(receiverName)
            receiver ! Player.PrintMessage(s"$playerName to $receiverName: $message")
            }catch{
                case e: ju.NoSuchElementException => sender ! Player.PrintMessage(s"$receiverName is not in the game.")
            }
        case m => println("Unhandled message in PlayerManager: " + m)
    }
}

object PlayerManager{
    case class CreatePlayer(playerName: String, playerRoom: String, sock: Socket, in:BufferedReader, out: PrintStream)
    case object CheckAllInputs
    case class TellPlayer(playerName: String, receiverName: String, message: String)
}