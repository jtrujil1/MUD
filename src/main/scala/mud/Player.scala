package mud

import scala.languageFeature.existentials

class Player(val name: String, private var inventory: List[Item], position: Room) {

    def processCommand(command: String, itemName:String = "None"): Unit =
        command match{
            case "look" => println(position.desc)
            case "inventory" => inventoryListing()
            case "inv" => inventoryListing()
            case "get" =>
            position.getItem(itemName)
            if(position.getItem(itemName) != None) addtoInventory(position.getItem(itemName))
            case "drop" =>
            if(getFromIventory(itemName) != None) position.dropItem(getFromInventory(itemName).get)
            case "help" => 
            case _ => move(command)
        }

    def getFromInventory(itemName: String): Option[Item] =
        inventory.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            inventory = inventory.filter(_ != item)//use patch instead to not drop everything that is the same
            printf("\n%s dropped in %s.", itemName, this.position.name)
            Some(item)
            case None =>
            printf("\nThere is no %s in inventory.", itemName)
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
        printf("\nInventory (total items: %d)", inventory.length)
        for(i <- 0 until inventory.length){
                println(inventory(i).name + " " + inventory(i).desc)
        }
        return "v"
    }

    def move(dir: String): Unit =
        dir match{
            case "north" =>
            case "south" =>
            case "east" =>
            case "west" =>
            case "up" =>
            case "down" =>
            case _ => println("Please enter a valid command. If you want to look at the available commands enter \"help\".")
        }
}

object Player {

}