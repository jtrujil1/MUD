package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import scala.collection.mutable

class NPCManager extends Actor {
    private var npcs = Map[String, ActorRef]()
    private var npcNames: mutable.Buffer[String] = mutable.Buffer("WEYLAN", "FEN", "ALMA", "MAE", "ALAN", "KIRA", "AZURA", "COLT")
    private var npcItem = Map[String, Item]()
    val katana = new Item("katana", 100, 30, "A single-edge blade. Very sharp, not too heavy.")
    val dagger = new Item("dagger", 60, 25, "A small but handy item, with a hilt carved out of bamboo.")
    val stick = new Item("meditation stick", 50, 18, "Wooden stick (no chain), works as a meditation aid.")
    val needles = new Item("needles", 45, 30, "Useful for ripped clothing but also in battle.")

    import NPCManager._
    def receive = {
        case CreateNPC =>
            var name = npcNames(0)
            npcNames -= name
            val newNPC = context.actorOf(Props(new NPC(name, List(katana, dagger, stick, needles))), name)
            npcs += (name -> newNPC)
            val npcRoom = util.Random.nextInt(6) match{
                case 0 => "Dojo"
                case 1 => "Underground"
                case 2 => "Village"
                case 3 => "Lake"
                case 4 => "Forest"
                case 5 => "House"
            }
            Main.roomManager ! RoomManager.AddPlayerToRoom(newNPC, npcRoom)
        case TellPlayer(player, receiverName, message) =>
            if(npcs.contains(receiverName.toUpperCase())){
                val receiver = npcs(receiverName.toUpperCase())
                receiver ! NPC.GetMessage(player, message)
            }else{
                player ! Player.PrintMessage(s"\n$receiverName is not in the game.\n")
            }
        case RemovePlayer => npcs = npcs-sender.path.name
        case m => println("Unhandled message in ActivityManager: " + m)
    }
}

object NPCManager{
    case object CreateNPC
    case class TellPlayer(player: ActorRef, receiverName: String, message: String)
    case object RemovePlayer
}
