package mud

import scala.languageFeature.existentials
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class NPC(val name: String, private var inventory: List[Item]) extends Actor {

    val defaultWeapon = new Item("fists", 45, 24, "last resort in a fight")
    private var position: ActorRef = null
    private var health = 300
    private var victim: Option[ActorRef] = None
    private var currentWeapon: Item = if(inventory.length > 0)inventory(util.Random.nextInt(inventory.length)) else defaultWeapon

    import Player._
    import NPC._
    def receive = {
        case PrintMessage(message) => message
        case TakeItem(item) =>
            if(item != None) addToInventory(item.get)
        case TakeExit(exit) =>
            if(exit != None){
                position ! Room.RemovePlayer
                position = exit.get
                position ! Room.AddPlayer
                if(victim != None){
                    victim.get ! Player.StopCombat
                    victim = None
                }
                Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(290)+200, self, NPC.AskForExit)
            }else position ! Room.GetExit(util.Random.nextInt(6))
        case AddPlayerToFirstRoom(room) =>
            position = room
            position ! Room.AddPlayer
            Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(290)+200, self, NPC.AskForExit)
        case AskForExit => if (victim == None) position ! Room.GetExit(util.Random.nextInt(6))
        case GetMessage(player, message) =>
            var droppedItem = false
            val messageBack = util.Random.nextInt(6) match {
                case 0 =>
                    if(message.contains("?")){
                        "I will not answer any of your questions!"
                    }else{
                        "You should try the mushrooms from the lake."
                    }
                case 1 => "If you see a pouch of silver coins, they're mine."
                case 2 => "I'll tell you a secret, I hate grilled corn."
                case 3 => "I'm so lost, where was the forest again?"
                case 4 => 
                    if(message.toLowerCase.contains("item") || message.toLowerCase.contains("items")){
                        val item = getFromInventory(inventory(util.Random.nextInt(inventory.length)).name)
                        if(item != None) position ! Room.DropItem(item.get)
                        if(item.get == currentWeapon) currentWeapon = inventory(util.Random.nextInt(inventory.length))
                        droppedItem = true
                        "Here, I'll leave you something. Seems like you need it."
                    }else{
                        "You know some people around here are really nice, they might give you something if you ask."
                    }
                case 5 => "I bet there's something hidden in the Dojo, it looks sketchy."
            }
            player ! Player.PrintMessage(s"\n$name to ${player.path.name}: $messageBack\n")
            if(droppedItem) position ! Room.PlayerDroppedItem(name, player)
        case HitPlayer(victim) => victim ! Player.GetHit(currentWeapon.damage, position)
        case GetHit(damage, room) =>
            if(room == position){
                health -= damage
                if(health > 0){
                    sender ! Player.HitResult(true, false)
                    if(victim == None || victim == Some(sender)){
                        victim = Some(sender)
                        var delay = 40 - currentWeapon.speed
                        Main.activityManager ! ActivityManager.ScheduleEvent(delay, self, Player.HitPlayer(victim.get))
                    }
                    if(health < 60) position ! Room.GetExit(util.Random.nextInt(6))
                }else{
                    sender ! Player.HitResult(true, true)
                    Main.npcManager ! NPCManager.RemovePlayer
                    position ! Room.RemovePlayer
                    if(currentWeapon != defaultWeapon) inventory = currentWeapon::inventory
                    if(inventory.length > 0){
                        inventory.foreach(item => position ! Room.DropItem(item))
                        position ! Room.PlayerDroppedItem(name, sender)
                    }
                    context.stop(self)
                }
            }else sender ! Player.HitResult(false, false)
        case HitResult(result, dead) =>
            if(!result){
                victim = None
            }
            if(dead){
                victim = None
            }
        case StopCombat => victim = None
        case m => println("Unhandled message in NPC: "+ m)
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
    }
}

object NPC {
    case class GetMessage(player: ActorRef, message: String)
    case object AskForExit
}
