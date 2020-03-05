package mud

import akka.actor.Actor
import akka.actor.ActorRef

class Room(val name: String, val desc: String, private var items: List[Item], val exitNames: Array[String]) extends Actor {
    
    import Room._
    def receive = {
        case LinkRooms(links) => exits = exitNames.map(links.get)
        case PrintDescription => sender ! Player.PrintMessage(description())
        case GetExit(dir) => sender ! Player.TakeExit(getExit(dir))
        case GetItem(itemName) => sender ! Player.TakeItem(getItem(itemName))
        case DropItem(item) => dropItem(item)
        case m => println("Unhandled message in Room: " + m)
        //case GetName => sender ! TakeName
    }

    private var exits: Array[Option[ActorRef]] = null

    def description(): String = {
        var descStr: String = ""
        descStr += s"$name\n$desc\nExits: "
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
        descStr
    }

    def getExit(dir: Int): Option[ActorRef] = {
        // if (exits(dir) != "-1") {
        //     println("You have moved to " + Room.rooms(exits(dir)).name + ".")
        //     Some(Room.rooms(exits(dir)))
        // } else {
        //     println("There is no exit that way, you can use look to see your available exits.")
        //     None
        // }
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
}
