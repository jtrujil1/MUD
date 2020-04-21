package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import java.io.BufferedReader
import java.io.PrintStream
import java.net.Socket
import java.{util => ju}

class PlayerManager extends Actor {

    private var players = Map[String, ActorRef]()

    import PlayerManager._
    def receive = {
        case CreatePlayer(playerName, playerRoom, sock, in, out) => 
            val newPlayer = context.actorOf(Props(new Player(playerName, sock, in, out, List())), playerName)
            players += (playerName.toLowerCase -> newPlayer)
            Main.roomManager ! RoomManager.AddPlayerToRoom(newPlayer, playerRoom)
        case CheckAllInputs =>
            for (child <- context.children) child ! Player.CheckInput
        case TellPlayer(playerName, receiverName, message) =>
            if(players.contains(receiverName.toLowerCase())){
                val receiver = players(receiverName.toLowerCase())
                receiver ! Player.PrintMessage(s"$playerName to $receiverName: $message")
            }else{
                Main.npcManager ! NPCManager.TellPlayer(sender, receiverName, message)
            }
        case RemovePlayer => players = players-sender.path.name.toLowerCase
        case m => println("Unhandled message in PlayerManager: " + m)
    }
}

object PlayerManager{
    case class CreatePlayer(playerName: String, playerRoom: String, sock: Socket, in:BufferedReader, out: PrintStream)
    case object CheckAllInputs
    case class TellPlayer(playerName: String, receiverName: String, message: String)
    case object RemovePlayer
}