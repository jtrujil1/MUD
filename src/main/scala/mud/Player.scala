package mud

import scala.languageFeature.existentials
import akka.actor.Actor
import akka.actor.ActorRef

class Player(var position: Room, private var inventory: List[Item], val name: String = "Player 1") extends Actor {

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
    }

    def processCommand(command: String): Unit = {
        val commandArray = command.split(" +", 2)
        commandArray(0).toLowerCase match{
            case "look" => println(position.description())
            case "inventory" | "inv" => println(inventoryListing())
            case "get" =>
            val item = position.getItem(commandArray(1))
            position ! Room.GetItem
            if(item != None) addToInventory(item.get)
            case "drop" =>
            val item2 = getFromInventory(commandArray(1))
            if(item2 != None) position.dropItem(item2.get)
            case "help" =>
            println("""All Commands:
            north, south, east, west, up, down - for movement (abbreviations also work)
            look - reprints the description of the current room
            inv/inventory - list the contents of your inventory
            get item - to get an item from the room and add it to your inventory
            drop item - to drop an item from your inventory into the room
            exit - leave the game
            help - print the available commands and what they do""")
            case "exit" => println("Thank you for playing.")
            case "north" | "n" => move(command)
            case "south" | "s" => move(command)
            case "east" | "e" => move(command)
            case "west" | "w" => move(command)
            case "up" | "u" => move(command)
            case "down" | "d" => move(command)
            case _ => println("Please enter a valid command. If you want to look at the available commands enter \"help\".")
        }
    }
    
    def getFromInventory(itemName: String): Option[Item] =
        inventory.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            inventory = inventory.patch(inventory.indexOf(item),Nil,1)
            printf("\n%s dropped in the %s.\n", itemName, this.position.name)
            Some(item)
            case None =>
            println(s"There is no $itemName in your inventory.")
            None
        }

    def addToInventory(item: Item): Unit = {
        inventory = item::inventory
        println()
        println(s"The item ${item.name} has been added to your inventory.")
    }

    def inventoryListing(): String = {
        var invString:String = ""
        invString += "Inventory:\n"
        for(i <- 0 until inventory.length){
                invString += ("     " + inventory(i).name + " - " + inventory(i).desc + "\n")
        }
        if(inventory.length == 0) invString += "No items in inventory."
        return invString
    }

    def move(dir: String): Unit = {
        var direction:Option[Room] = None
        dir match{
            case "north" | "n" =>
            direction = position.getExit(0)
            if(direction != None)
                position = direction.get
            case "south" | "s" =>
            direction = position.getExit(1)
            if(direction != None)
                position = direction.get
            case "east" | "e" =>
            direction = position.getExit(2)
            if(direction != None)
                position = direction.get
            case "west" | "w" =>
            direction = position.getExit(3)
            if(direction != None)
                position = direction.get
            case "up" | "u" =>
            direction = position.getExit(4)
            if(direction != None)
                position = direction.get
            case "down" | "d" =>
            direction = position.getExit(5)
            if(direction != None)
                position = direction.get
                println("You have moved to " + position.name + ".")
                position ! Room.PrintDescription
                
        }
    }
}

object Player {
    case class CheckInput(input: String)
    case class PrintMessage(msg: String)
    case class TakeExit(exit: Option[ActorRef])
    case class TakeItem(item: Option[Item])

    case class MoveRooms(dir: String)
    case class PrintInventory()
    case class AddItem(item: Item)
    case class GetFromInv(itemName: String)
}