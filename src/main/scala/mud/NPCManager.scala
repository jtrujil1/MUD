package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import scala.collection.mutable

class NPCManager extends Actor {
    val npcs = mutable.Map[String, ActorRef]()
    var npcCount = 0
    var npcNames: mutable.Buffer[String] = mutable.Buffer("WEYLAN", "FEN", "ALMA", "MAE", "ALAN", "KIRA", "AZURA", "COLT")
    var npcItem = Map[String, Item]()
    var katana = new Item("katana", "A single-edge blade. Very sharp, not too heavy.")

    import NPCManager._
    def receive = {
        case CreateNPC =>
            var name = npcNames(0)
            npcNames -= name
            val newNPC = context.actorOf(Props(new NPC(name, List(katana))), name)
            npcs += (name -> newNPC)
            val npcRoom = util.Random.nextInt(6) match{
                case 0 => "Dojo"
                case 1 => "Underground"
                case 2 => "Village"
                case 3 => "Lake"
                case 4 => "Forest"
                case 5 => "House"
            }
            Main.roomManager ! RoomManager.AddNPCToRoom(newNPC, npcRoom)
            npcCount += 1
        case TellPlayer(player, receiverName, message) =>
            if(npcs.contains(receiverName.toUpperCase())){
                val receiver = npcs(receiverName.toUpperCase())
                receiver ! NPC.GetMessage(player, message)
            }else{
                player ! Player.PrintMessage(s"\n$receiverName is not in the game.\n")
            }
        case m => println("Unhandled message in ActivityManager: " + m)
    }
}

object NPCManager{
    case object CreateNPC
    case class TellPlayer(player: ActorRef, receiverName: String, message: String)
}
