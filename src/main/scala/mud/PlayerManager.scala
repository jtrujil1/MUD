package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import java.io.BufferedReader
import java.io.PrintStream

class PlayerManager extends Actor {

    import PlayerManager._
    def receive = {
        case CreatePlayer(playerName, playerRoom, in, out) => 
            val newPlayer = context.actorOf(Props(new Player(playerName, in, out, List())), playerName)
            Main.roomManager ! RoomManager.AddPlayerToRoom(newPlayer, playerRoom)
        case CheckAllInputs =>
            for (child <- context.children) child ! Player.CheckInput
        case m => println("Unhandled message in PlayerManager: " + m)
    }
}

object PlayerManager{
    case class CreatePlayer(playerName: String, playerRoom: String, in:BufferedReader, out: PrintStream)
    case object CheckAllInputs
}