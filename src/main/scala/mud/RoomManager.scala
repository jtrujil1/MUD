package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import scala.collection.mutable

class RoomManager extends Actor {

    val roomExits = mutable.Map[String, Array[String]]()

    import RoomManager._
    def receive = {
        case AddPlayerToRoom(player, key) =>
            val room:ActorRef = rooms.get(key).get
            player ! Player.AddPlayerToFirstRoom(room)
        case ShortestPath(currentRoom, destination) =>
            if(currentRoom.path.name == destination){
                sender ! Player.PrintMessage(s"You are already in $destination.")
            // else if(!room.contains(destination))
            }else{
                val directions = shortestPath(currentRoom.path.name.toLowerCase, destination.toLowerCase, Set[String](), List[String]())
                sender ! Player.PrintMessage("Steps: " + directions.length.toString)
                var directionString = s"\nShortest path to the $destination:\n"
                for (i <- 0 until directions.length) {
                    directionString += directions(i) + "\n"
                }
                sender ! Player.PrintMessage(s"$directionString")
            }
        case m => println("Unhandled message in RoomManager: " + m)
    }

    val rooms = readRooms()
    for(child <- context.children) child ! Room.LinkRooms(rooms)

    def readRooms(): Map[String, ActorRef] = {
        val xmlData = xml.XML.loadFile("world.xml")
        (xmlData \ "room").map(readRoom).toMap
    }

    def readRoom(node: xml.Node): (String, ActorRef) = {
        val name = (node \ "@name").text
        val keyword = (node \ "@keyword").text
        val desc = (node \ "desc").text
        val items = (node \ "item").map(n => (Item((n \ "@name").text, (n \ "@damage").text.toInt, (n \ "@speed").text.toInt, n.text))).toList
        var exits = (node \ "exits").text.split(",").map(_.toString)
        roomExits(keyword.toLowerCase) = exits.map(_.toLowerCase)
        keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
    }

    val directions = Array("north", "south", "east", "west", "up", "down")

    def shortestPath(curr:String, dest:String, visited:Set[String], path: List[String]): List[String] = {
        val newVisited = visited + curr
        if(curr == dest) List[String]()
        else{
            val pathLength = for(i <- 0 until roomExits(curr).length; if(!visited(roomExits(curr)(i)) && roomExits(curr)(i) != "-1")) yield{
                List(directions(i)) ::: shortestPath(roomExits(curr)(i), dest, newVisited, path :+ directions(i))
            }
            if(pathLength.length == 0 && newVisited.size > 1) List.fill(100)("a")
            else{
                pathLength.minBy(_.size)
            }
        }
    }
}

object RoomManager {
    case class AddPlayerToRoom(player: ActorRef, keyword: String)
    case class ShortestPath(currentRoom: ActorRef, destination: String)
}