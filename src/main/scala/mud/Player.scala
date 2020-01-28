package mud

import scala.languageFeature.existentials

class Player(val name: String, private var inventory: List[Item], position: Room) {

    def processCommand(command: String, itemName:String = "None"): Unit =
        command match{
            case "look" => println(position.desc)
            case ("inventory" | "inv") => inventoryListing()
            case "get" =>
            position.getItem(itemName)
            //if(position.getItem(itemName) != None) addtoInventory(position.getItem(itemName))
            case "drop" =>
           // if(getFromIventory(itemName) != None) position.dropItem(getFromInventory(itemName).get)
            case "help" =>
            println(""""All Commands:
            north, south, east, west, up, down - for movement (abbreviations also work)
            look - reprints the description of the current room
            inv/inventory - list the contents of your inventory
            get item - to get an item from the room and add it to your inventory
            drop item - to drop an item from your inventory into the room.
            exit - leave the game
            help - print the available commands and what they do
            """")
            case _ => move(command)
        }

    def getFromInventory(itemName: String): Option[Item] =
        inventory.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            inventory = inventory.filter(_ != item)//use patch instead to not drop everything that is the same
            printf("\n%s dropped in the %s.", itemName, this.position.name)
            Some(item)
            case None =>
            println(s"There is no $itemName in your inventory.")
            None
        }

    def addToInventory(item: Item): Unit = {
        if(position.getItem(item.name) != None){
            position.getItem(item.name)::inventory
            println()
            printf("\n%s has been added to your inventory.", item)
        }else{
            println("That is not a valid item.")
        }
    }

    def inventoryListing(): String = {
        var invString:String = ""
        invString += ("Inventory (total items: %d)", inventory.length)
        for(i <- 0 until inventory.length){
                invString += ("     " + inventory(i).name + " - " + inventory(i).desc)
        }
        return invString
    }

    def move(dir: String): Unit =
        dir match{
            case "north" | "n" => 
            case "south" | "s" =>
            case "east" | "e" =>
            case "west" | "w" =>
            case "up" | "u" =>
            case "down" | "d" =>
            case _ => println("Please enter a valid command. If you want to look at the available commands enter \"help\".")
        }
}

object Player {

}