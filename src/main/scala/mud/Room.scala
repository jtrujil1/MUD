package mud

import akka.actor.Actor
import akka.actor.ActorRef

class Room(val name: String, val desc: String, private var items: List[Item], private val exits: Array[String]) extends Actor {

    def receive = {
        case m => println("Unhandled message in Room: " + m)
    }

    def description(): String = {
        var descStr:String = ""
        descStr += s"$name\n$desc\nExits: "
        var counter = 0
        for (i <- 0 to 5){
            if(exits(i) != "-1"){
                var exit = ""
                i match{
                    case 0 => exit = "north"
                    case 1 => exit = "south"
                    case 2 => exit = "east"
                    case 3 => exit = "west"
                    case 4 => exit = "up"
                    case 5 => exit = "down"
                }
                if(counter > 0) descStr += ", "
                descStr += exit
                counter += 1 
            }
        }
        
        descStr += "\nItems: "
        for (i <- 0 until this.items.length){
            descStr += items(i).name
            if(i < items.length-1) descStr += ", "
        }
        descStr
    }

    def getExit(dir: Int): Option[Room] = {
        if(exits(dir) != "-1"){
            println("You have moved to " + Room.rooms(exits(dir)).name + ".")
            Some(Room.rooms(exits(dir)))
        }else{
            println("There is no exit that way, you can use look to see your available exits.")
            None
        }
    }

    def getItem(itemName: String): Option[Item] = {
        items.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            items = items.patch(items.indexOf(item),Nil,1)
            Some(item)
            case None => None
        }
    }

    def dropItem(item: Item): Unit = items ::= item
}

object Room {

}