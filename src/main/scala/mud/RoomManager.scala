package mud

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props

class RoomManager extends Actor {

    def receive = {
        case m => println("Unhandled message in RoomManager: " + m)
    }

    val rooms = readRooms()

    def readRooms(): Map[String, ActorRef] = {
        val xmlData = xml.XML.loadFile("world.xml")
        (xmlData \ "room").map(readRoom).toMap
    }

    def readRoom(node: xml.Node): (String, ActorRef) = {
        val name = (node \ "@name").text
        val keyword = (node \ "@keyword").text
        val desc = (node \ "description").text
        val items = (node \ "item").map(n => (Item((n \ "@name").text, n.text))).toList
        val exits = (node \ "exits").text.split(",").map(_.toString)
        keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
    }
}