package mud

import akka.actor.Actor
import akka.actor.ActorRef

class Room(val name: String, val desc: String, private var items: List[Item], val exitNames: Array[String]) extends Actor {

    private var playersInRoom= new mud.MutableDLList[ActorRef]()
    private var npcsInRoom= new mud.MutableDLList[ActorRef]()
    
    import Room._
    def receive = {
        case LinkRooms(links) => exits = exitNames.map(links.get)
        case PrintDescription => sender ! Player.PrintMessage(description())
        case GetExit(dir) =>
            val exit = getExit(dir)
            sender ! Player.TakeExit(exit)
            if(exit != None) playersInRoom += sender()
        case GetItem(itemName) => sender ! Player.TakeItem(getItem(itemName))
        case DropItem(item) => dropItem(item)
        case SayMessage(playerName, msg) => playersInRoom.foreach(_ ! Player.PrintMessage(s"\n$playerName: $msg\n"))
        case AddPlayer => 
            playersInRoom.foreach(_ ! Player.PrintMessage(s"\n${sender.path.name} has entered the room.\n"))
            playersInRoom += sender
            sender ! Player.PrintMessage(s"\nYou have moved to the $name.\n" + description())
        case AddNPC =>
            playersInRoom.foreach(_ ! Player.PrintMessage(s"\n${sender.path.name} has entered the room.\n"))
            npcsInRoom += sender
        case RemovePlayer => 
            playersInRoom = playersInRoom.filter(_ != sender())
            playersInRoom.foreach(_ ! Player.PrintMessage(s"\n${sender.path.name} has left the room.\n"))
        case RemoveNPC => 
            npcsInRoom = npcsInRoom.filter(_ != sender())
            playersInRoom.foreach(_ ! Player.PrintMessage(s"\n${sender.path.name} has left the room.\n"))
        case GetNPCExit(dir) => 
            val exit = getExit(dir)
            sender ! NPC.TakeExit(exit)
            if(exit != None) npcsInRoom += sender()
        case NPCDroppedItem(npcName, player) => player ! Player.PrintMessage(s"\n$npcName has dropped an item in the $name.\n")
        case m => println("Unhandled message in Room: " + m)
    }

    private var exits: Array[Option[ActorRef]] = null

    def description(): String = {
        var descStr: String = ""
        descStr += s"\n$desc\nExits: "
        var counter = 0
        for (i <- 0 to 5) {
            if (exitNames(i) != "-1") {
                var exit = ""
                i match {
                    case 0 => exit = "north"
                    case 1 => exit = "south"
                    case 2 => exit = "east"
                    case 3 => exit = "west"
                    case 4 => exit = "up"
                    case 5 => exit = "down"
                }
                if (counter > 0) descStr += ", "
                descStr += exit
                counter += 1
            }
        }

        descStr += "\nItems: "
        for (i <- 0 until this.items.length) {
            descStr += items(i).name
            if (i < items.length - 1) descStr += ", "
        }
        descStr += "\nPlayers: "
        for (i <- 0 until playersInRoom.length) {
            descStr += playersInRoom(i).path.name
            if (i < playersInRoom.length - 1) descStr += ", "
        }
        if(!npcsInRoom.isEmpty) descStr += ", "
        for (i <- 0 until npcsInRoom.length) {
            descStr += npcsInRoom(i).path.name
            if (i < npcsInRoom.length - 1) descStr += ", "
        }
        descStr + "\n"
    }

    def getExit(dir: Int): Option[ActorRef] = {
        exits(dir)
    }

    def getItem(itemName: String): Option[Item] = {
        items.find(_.name.toLowerCase == itemName.toLowerCase) match {
        case Some(item) =>
            items = items.patch(items.indexOf(item), Nil, 1)
            Some(item)
        case None => None
        }
    }

    def dropItem(item: Item): Unit = items ::= item
}

object Room {
    case class LinkRooms(links: Map[String, ActorRef])
    case object PrintDescription
    case class GetExit(dir: Int)
    case class GetItem(itemName: String)
    case class DropItem(item: Item)
    case class SayMessage(playerName: String, msg: String)
    case object AddPlayer
    case object RemovePlayer
    case class GetNPCExit(dir: Int)
    case object AddNPC
    case object RemoveNPC
    case class NPCDroppedItem(npcName: String, player: ActorRef)
}
