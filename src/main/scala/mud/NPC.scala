package mud

import scala.languageFeature.existentials
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class NPC(val name: String, private var inventory: List[Item]) extends Actor {

    private var position: ActorRef = null

    import NPC._
    def receive = {
        case TakeItem(item) =>
            if(item != None) addToInventory(item.get)
        case TakeExit(exit) =>
            if(exit != None){
                position ! Room.RemoveNPC
                position = exit.get
                position ! Room.AddNPC
                Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(30)+250, self, NPC.AskForExit)
            }else position ! Room.GetNPCExit(util.Random.nextInt(6))
        case AddNPCToFirstRoom(room) =>
            position = room
            position ! Room.AddNPC
            Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(30)+250, self, NPC.AskForExit)
        case AskForExit => position ! Room.GetNPCExit(util.Random.nextInt(6))
        case GetMessage(player, message) =>
            var droppedItem = false
            val messageBack = util.Random.nextInt(6) match {
                case 0 =>
                    if(message.contains("?")){
                        "I will not answer any of your questions!"
                    }else{
                        "You should try the mushrooms from the lake."
                    }
                case 1 => "If you see a pouch of silver coins, they're mine."
                case 2 => "I'll tell you a secret, I hate grilled corn."
                case 3 => "I'm so lost, where was the forest again?"
                case 4 => 
                    if(message.toLowerCase.contains("item") || message.toLowerCase.contains("items")){
                        val item = getFromInventory("katana")
                        if(item != None) position ! Room.DropItem(item.get)
                        droppedItem = true
                        "Here, I'll leave you my katana. Seems like you need it."
                    }else{
                        "You know some people around here are really nice, they might give you something if you ask."
                    }
                case 5 => "I bet there's something hidden in the Dojo, it looks sketchy."
            }
            player ! Player.PrintMessage(s"\n$name to ${player.path.name}: $messageBack\n")
            if(droppedItem) position ! Room.NPCDroppedItem(name, player)
        case m => println("Unhandled message in NPC: "+ m)
    }

    def getFromInventory(itemName: String): Option[Item] = {
        inventory.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
                inventory = inventory.patch(inventory.indexOf(item),Nil,1)
                Some(item)
            case None => 
                None
        }
    }

    def addToInventory(item: Item): Unit = {
        inventory = item::inventory
    }
}

object NPC {
    case class TakeExit(exit: Option[ActorRef])
    case class TakeItem(item: Option[Item])
    case class AddNPCToFirstRoom(room: ActorRef)
    case class GetMessage(player: ActorRef, message: String)
    case object AskForExit
}
