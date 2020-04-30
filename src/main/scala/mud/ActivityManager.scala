
package mud

import akka.actor.Actor
import akka.actor.ActorRef

class ActivityManager extends Actor {
    import ActivityManager._
    val pq = new mud.BinaryHeap[Event](_.time < _.time)
    private var now = 0

    def receive = {
        case CheckQueue =>
            while(!pq.isEmpty && pq.peek.time <= now){
                var event = pq.dequeue
                event.recipient ! event.msg
            }
            now += 1
        case ScheduleEvent(delay, recipient, msg) => pq.enqueue(new Event(now + delay, recipient, msg))
        case m => println("Unhandled message in ActivityManager: " + m)
    }
}

object ActivityManager {
    case class Event(time: Int, recipient: ActorRef, msg: Any)
    case object CheckQueue
    case class ScheduleEvent(delay: Int, recipient: ActorRef, msg: Any)
}