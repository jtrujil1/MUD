package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props

class PlayerManager extends Actor {

    import PlayerManager._
    def receive = {
        case m => println("Unhandled message in PlayerManager: " + m)
        case CreatePlayer(playerName) => context.actorOf(Props(new Player(nil, playerName), playerName))
    }
}

object PlayerManager{
    case class CreatePlayer(playerName: String)
}
//printStream,bufferedReader