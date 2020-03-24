package mud

import scala.languageFeature.existentials
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.BufferedReader
import java.io.PrintStream

class Player(val name: String, val in: BufferedReader, val out: PrintStream, private var inventory: List[Item]) extends Actor {
    private var position: ActorRef = null

    import Player._
    def receive = {
        case CheckInput =>
            if(in.ready) {
                val input = in.readLine()
                processCommand(input)
            }
        case PrintMessage(msg) => println(msg)
        case TakeItem(item) =>
            if(item != None) addToInventory(item.get)
            else self ! PrintMessage("That item is not in the room.")
        case TakeExit(exit) =>
            if(exit != None){
                position = exit.get
                position ! Room.GetRoomName
            }else{
                self ! PrintMessage("There is no exit that way.")
            }
        case MoveRooms(command) => move(command)
        case ProcessCommand(command) => processCommand(command)
        case GetFromInv(itemName) => self ! BackFromInv(getFromInventory(itemName))
        case BackFromInv(item) =>
            if(item != None){
                position ! Room.DropItem(item.get)
                self ! PrintMessage(s"You have dropped the ${item.get.name} in the ${position.path.name}.")
            }else{
                self ! PrintMessage("That item is not in your inventory.")
            }
        case TakeRoomName(name) =>
            self ! PrintMessage(s"You have moved to the $name.")
            position ! Room.PrintDescription
    }

    def processCommand(command: String): Unit = {
        val commandArray = command.split(" +", 2)
        commandArray(0).toLowerCase match{
            case "look" => position ! Room.PrintDescription
            case "inventory" | "inv" => self ! PrintMessage(inventoryListing())
            case "get" =>
                position ! Room.GetItem(commandArray(1))
            case "drop" =>
                self ! GetFromInv(commandArray(1))
            case "help" =>
                self ! PrintMessage("""All Commands:
                north, south, east, west, up, down - for movement (abbreviations also work)
                look - reprints the description of the current room
                inv/inventory - list the contents of your inventory
                get item - to get an item from the room and add it to your inventory
                drop item - to drop an item from your inventory into the room
                exit - leave the game
                help - print the available commands and what they do""")
            case "exit" =>
                self ! PrintMessage ("Thank you for playing.")
                context.system.terminate()
            case "north" | "n" => self ! MoveRooms(command)
            case "south" | "s" => self ! MoveRooms(command)
            case "east" | "e" => self ! MoveRooms(command)
            case "west" | "w" => self ! MoveRooms(command)
            case "up" | "u" => self ! MoveRooms(command)
            case "down" | "d" => self ! MoveRooms(command)
            case _ => self ! PrintMessage("Please enter a valid command. If you want to look at the available commands enter \"help\".") 
        }
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
        self ! PrintMessage(s"\nThe item ${item.name} has been added to your inventory.")
    }

    def inventoryListing(): String = {
        var invString:String = ""
        invString += "Inventory:\n"
        for(i <- 0 until inventory.length){
                invString += ("     " + inventory(i).name + " - " + inventory(i).desc.trim + "\n")
        }
        if(inventory.length == 0) invString += "No items in inventory."
        return invString
    }

    def move(dir: String): Unit = {
        dir match{
            case "north" | "n" =>
                position ! Room.GetExit(0)
            case "south" | "s" =>
                position ! Room.GetExit(1)
            case "east" | "e" =>
                position ! Room.GetExit(2)
            case "west" | "w" =>
                position ! Room.GetExit(3)
            case "up" | "u" =>
                position ! Room.GetExit(4)
            case "down" | "d" =>
                position ! Room.GetExit(5)
        }
    }
}

object Player {
    case class CheckInput(input: String)
    case class PrintMessage(msg: String)
    case class TakeExit(exit: Option[ActorRef])
    case class TakeItem(item: Option[Item])
    case class ProcessCommand(command: String)
    case class MoveRooms(dir: String)
    case class AddItem(item: Item)
    case class GetFromInv(itemName: String)
    case class BackFromInv(item: Option[Item])
    case class TakeRoomName(name: String)
}