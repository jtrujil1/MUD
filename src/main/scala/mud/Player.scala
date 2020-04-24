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

    val defaultWeapon:Item = new Item("fists", 45, 24, "last resort in a fight")
    private var position: ActorRef = null
    private var health: Int = 300
    private var currentWeapon: Item = defaultWeapon
    private var victim: Option[ActorRef] = None

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
            else out.println("\nThat item is not in the room.\n")
        case TakeExit(exit) =>
            if(exit != None){
                position ! Room.RemovePlayer
                position = exit.get
                position ! Room.AddPlayer
                if(victim != None){
                    victim.get ! Player.StopCombat
                    victim = None
                }
            }else{
                out.println("\nThere is no exit that way.\n")
            }
        case AddPlayerToFirstRoom(room) =>
            position = room
            position ! Room.AddPlayer
        case GetHit(damage, room) =>
            if(room == position){
                health -= damage
                if(health > 0){
                    out.println(s"\nYou've been hit. You're health is now at $health.\n")
                    sender ! Player.HitResult(true, false)
                    if(victim == None || victim == Some(sender)){
                        victim = Some(sender)
                        var delay = 40 - currentWeapon.speed
                        Main.activityManager ! ActivityManager.ScheduleEvent(delay, self, Player.HitPlayer(victim.get))
                    }
                }else{
                    out.println("\nYou've been killed. Game over.\n")
                    sender ! Player.HitResult(true, true)
                    Main.playerManager ! PlayerManager.RemovePlayer
                    position ! Room.RemovePlayer
                    if(currentWeapon != defaultWeapon) inventory = currentWeapon::inventory
                    if(inventory.length > 0){
                        inventory.foreach(item => position ! Room.DropItem(item))
                        position ! Room.PlayerDroppedItem(name, sender)
                    }
                    sock.close()
                    context.stop(self)
                }
            }else sender ! Player.HitResult(false, false)
            
        case FoundPlayer(fighter) =>
            if(fighter != None){
                victim = fighter
                var delay = 40 - currentWeapon.speed
                Main.activityManager ! ActivityManager.ScheduleEvent(delay, self, Player.HitPlayer(victim.get))
            }else out.println("\nThat player is not in the current room.\n")
        case HitPlayer(victim) => victim ! Player.GetHit(currentWeapon.damage, position)
        case HitResult(result, dead) =>
            if(result){
                out.println(s"\nYou've hit ${sender.path.name}.\n")
            }else{
                out.println("\nYou've missed,the player is not in the room anymore.\n")
                victim = None
            }
            if(dead){
                out.println(s"\n${sender.path.name} is now dead.\n")
                victim = None
            }
        case StopCombat =>
            victim = None
            out.println("\nPlayer has run away, combat has ended.\n")
        case m => println("Unhandled message in Player: "+ m)
    }


    def processCommand(command: String): Unit = {
        val commandArray = command.split(" +", 2)
        commandArray(0).toLowerCase match{
            case "look" =>
                if(victim == None){
                    position ! Room.PrintDescription
                }else out.println("\nYou cannot do this during combat.\n")
            case "inventory" | "inv" => 
                if(victim == None){
                    out.println(inventoryListing())
                }else out.println("\nYou cannot do this during combat.\n")
            case "get" =>
                if(victim == None){
                    position ! Room.GetItem(commandArray(1).trim)
                } else out.println("\nYou cannot do this during combat.\n")
            case "drop" =>
                if(victim == None){
                    val item = getFromInventory(commandArray(1).trim)
                    if(item != None){
                        position ! Room.DropItem(item.get)
                        out.println(s"\nYou have dropped the ${item.get.name} in the ${position.path.name}.\n")
                    }else{
                        out.println("\nThat item is not in your inventory.\n")
                    }
                }else out.println("\nYou cannot do this during combat.\n")
            case "help" =>
                out.println("""All Commands:
                north, south, east, west, up, down - for movement (abbreviations also work)
                look - reprints the description of the current room
                inv/inventory - list the contents of your inventory
                get *item* - to get an item from the room and add it to your inventory
                drop *item* - to drop an item from your inventory into the room
                say *message* - to communicate with everyone in your current room
                tell *player* *message* - to communicate with a user anywhere in the MUD
                listrooms - to list the names of all the available rooms
                shortestpath *room* - to list the directions for the shortest path to the room
                equip/unequip *item* - to pick which weapon in your inventory you are currently using
                kill *player* - to initiate combat
                flee/run away - to flee combat 
                exit - leave the game
                help - print the available commands and what they do
                do not forget, while in combat you can only fight or flee""")
            case "exit" =>
                position ! Room.RemovePlayer
                out.println("\nThank you for playing.\n")
                sock.close()
                context.stop(self)
            case "north" | "n" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "south" | "s" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "east" | "e" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "west" | "w" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "up" | "u" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "down" | "d" => if(victim != None) out.println("\nYou cannot leave a fight this way. Try rurnning away.\n") else move(command)
            case "say" => 
                if(victim == None){
                    val msg = commandArray(1)
                    position ! Room.SayMessage(name, msg)
                }else out.println("\nYou cannot do this during combat.\n")
            case "tell" => 
                if(victim == None){
                    val receiver = commandArray(1).split(" +", 2)(0)
                    val msg = commandArray(1).split(" +", 2)(1)
                    Main.playerManager ! PlayerManager.TellPlayer(name, receiver, msg)
                }else out.println("\nYou cannot do this during combat.\n")
            case "shortestpath" =>
                if(victim == None){
                    val destination = commandArray(1)
                    Main.roomManager ! RoomManager.ShortestPath(position, destination)
                }else out.println("\nYou cannot do this during combat.\n")
            case "listrooms" | "list" =>
                if(victim == None){
                    out.println("\nRooms:\n- Dojo\n- Village\n- Lake\n- Forest\n- House")
                }else out.println("\nYou cannot do this during combat.\n")
            case "equip" =>
                if(victim == None){
                    val weapon = getFromInventory(commandArray(1).trim)
                    if(weapon != None){
                        if(currentWeapon != defaultWeapon){
                            addToInventory(currentWeapon)
                        }
                        currentWeapon = weapon.get
                        out.println(s"\nYou are now using ${currentWeapon.name}.\n")
                    } else out.println("\nThat item is not in your inventory.\n")
                }else out.println("\nYou cannot do this during combat.\n")
            case "unequip" =>
                if(victim == None){
                    if(commandArray.length > 1){
                        val weapon = commandArray(1).trim
                        if(weapon == currentWeapon.name){
                            addToInventory(currentWeapon)
                            currentWeapon = defaultWeapon
                        } else out.println("\nYou are not currently using that weapon.\n")
                    }else if(currentWeapon != defaultWeapon){
                        addToInventory(currentWeapon)
                        currentWeapon = defaultWeapon
                    }
                }else out.println("\nYou cannot do this during combat.\n")
            case "kill" =>
                if(victim == None){
                    val fighter = commandArray(1).trim
                    position ! Room.GetPlayer(fighter)
                }else out.println("\nYou cannot do this during combat.\n")
            case "flee" | "run" => if(victim != None) position ! Room.GetExit(util.Random.nextInt(6)) else print("\nYou are not currently in battle.\n")
            case "health" => out.println(s"\nCurrent health: $health\n")
            case _ => out.println("\nPlease enter a valid command. If you want to look at the available commands enter \"help\".\n") 
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
        out.println(s"\nThe item ${item.name} has been added to your inventory.\n")
    }

    def inventoryListing(): String = {
        var invString:String = ""
        invString += "\nInventory:\n"
        for(i <- 0 until inventory.length){
                invString += ("     " + inventory(i).name + " - " + inventory(i).desc.trim + "\n")
        }
        if(inventory.length == 0) invString += "No items in inventory."
        return invString + "\n"
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
    case class GetHit(damage: Int, room: ActorRef)
    case class HitPlayer(victim: ActorRef)
    case class FoundPlayer(victim: Option[ActorRef])
    case class HitResult(result: Boolean, dead: Boolean)
    case object StopCombat
}