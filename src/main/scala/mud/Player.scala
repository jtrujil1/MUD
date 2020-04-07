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
import java.net.Socket

class Player(val name: String, 
            val sock: Socket, 
            val in: BufferedReader, 
            val out: PrintStream, 
            private var inventory: List[Item]) extends Actor {

    private var position: ActorRef = null

    import Player._
    def receive = {
        case CheckInput =>
            if(in.ready) {
                val input = in.readLine()
                processCommand(input)
            }
        case PrintMessage(msg) => out.println(msg)
        case TakeItem(item) =>
            if(item != None) addToInventory(item.get)
            else out.println("That item is not in the room.")
        case TakeExit(exit) =>
            if(exit != None){
                position ! Room.RemovePlayer
                position = exit.get
                position ! Room.AddPlayer
            }else{
                out.println("There is no exit that way.")
            }
        case AddPlayerToFirstRoom(room) =>
            position = room
            position ! Room.AddPlayer
    }

    def processCommand(command: String): Unit = {
        val commandArray = command.split(" +", 2)
        commandArray(0).toLowerCase match{
            case "look" => position ! Room.PrintDescription
            case "inventory" | "inv" => out.println(inventoryListing())
            case "get" =>
                position ! Room.GetItem(commandArray(1))
            case "drop" =>
                val item = getFromInventory(commandArray(1))
                if(item != None){
                    position ! Room.DropItem(item.get)
                    out.println(s"You have dropped the ${item.get.name} in the ${position.path.name}.")
                }else{
                    out.println("That item is not in your inventory.")
            }
            case "help" =>
                out.println("""All Commands:
                north, south, east, west, up, down - for movement (abbreviations also work)
                look - reprints the description of the current room
                inv/inventory - list the contents of your inventory
                get item - to get an item from the room and add it to your inventory
                drop item - to drop an item from your inventory into the room
                say message - to communicate with everyone in your current room
                tell user message - to communicate with a user anywhere in the MUD
                exit - leave the game
                help - print the available commands and what they do""")
            case "exit" =>
                position ! Room.RemovePlayer
                out.println("Thank you for playing.")
                context.system.terminate()
            case "north" | "n" => move(command)
            case "south" | "s" => move(command)
            case "east" | "e" => move(command)
            case "west" | "w" => move(command)
            case "up" | "u" => move(command)
            case "down" | "d" => move(command)
            case "say" => 
                val msg = commandArray(1)
                position ! Room.SayMessage(name, msg)
            case "tell" => 
                val receiver = commandArray(1).split(" +", 2)(0)
                val msg = commandArray(1).split(" +", 2)(1)
                Main.playerManager ! PlayerManager.TellPlayer(name, receiver, msg)
            case _ => out.println("Please enter a valid command. If you want to look at the available commands enter \"help\".") 
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
        out.println(s"\nThe item ${item.name} has been added to your inventory.")
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
    case class AddPlayerToFirstRoom(room: ActorRef)
}