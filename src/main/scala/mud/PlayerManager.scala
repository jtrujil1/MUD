package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props

class PlayerManager extends Actor {

    def receive = {
        case m => println("Unhandled message in PlayerManager: " + m)
    }
}