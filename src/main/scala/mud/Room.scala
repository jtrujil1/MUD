package mud

class Room(val name: String, val desc: String, private var items: List[Item], private val exits: Array[Int]) {
    def description(): String = {
        var descStr:String = ""
        s""""$name
        $desc
        Exits: 
        """"
        var counter = 0
        for (i <- 0 to 5){
            if(exits(i) != -1){
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
        for (i <- 0 until items.length){
            descStr += items(i).name
            if(i < items.length-1) descStr += ", "
        }
        descStr
    }

    def getExit(dir: Int): Option[Room] = {
        dir match{
            case "north" | "n" =>
                if(exits(0) != -1)
                    Some(Room.rooms(exits(0)))
                else
                    None
            case "south" | "s" =>
                if(exits(1) != -1)
                    Some(Room.rooms(exits(1)))
                else
                    None
            case "east" | "e" =>
                if(exits(2) != -1)
                    Some(Room.rooms(exits(2)))
                else
                    None
            case "west" | "w" =>
                if(exits(3) != -1)
                    Some(Room.rooms(exits(3)))
                else
                    None
            case "up" | "u" =>
                if(exits(4) != -1)
                    Some(Room.rooms(exits(4)))
                else
                    None
            case "down" | "d" =>
                if(exits(5) != -1)
                    Some(Room.rooms(exits(5)))
                else
                    None
        }
        inventory.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            inventory = inventory.filter(_ != item)//use patch instead to not drop everything that is the same
            printf("\n%s dropped in the %s.", itemName, this.position.name)
            Some(item)
            case None =>
            println(s"There is no $itemName in your inventory.")
            None
    }

    def getItem(itemName: String): Option[Item] = {
        items.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
            //items = items.filter(_ != item)//use patch instead to not drop everything that is the same
            items = items.patch(items.indexOf(item),Nil,1)
            Some(item)
            case None => None
        }
    }

    def dropItem(item: Item): Unit = items ::= item
}

object Room {
    val rooms = readRooms()
    def readRooms(): Array[Room] = {
        val source = scala.io.Source.fromFile("World.txt")
        val lines = source.getLines()
        val r = Array.fill(lines.next.toInt)(readRoom(lines))
        source.close()
        r
    }

    def readRoom(lines: Iterator[String]): Room = {
      val name = lines.next()
      val desc = lines.next()
      val items = List.fill(lines.next().toInt)(Item(lines.next(), lines.next()))
      val exits = lines.next().split(",").map(_.toInt)
      new Room(name, desc, items, exits)
    }
}