package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props

class PlayerManager extends Actor {

    import PlayerManager._
    def receive = {
        case m => println("Unhandled message in PlayerManager: " + m)
        case CreatePlayer(player, playerName) => context.actorOf(Props(player, playerName))
    }
}

object PlayerManager{
    case class CreatePlayer(player: ActorRef, playerName: String)
}
//printStream,bufferedReader