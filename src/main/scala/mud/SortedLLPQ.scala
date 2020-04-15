package mud

import reflect.ClassTag

class SortedLLPQ[A : ClassTag](higherP: (A, A) => Boolean) extends MyPriorityQueue[A] {
    private var default: A = _
    private class Node(val data: A, var prev: Node, var next: Node)
    private val end = new Node(default, null, null)
    end.next = end
    end.prev = end

    def enqueue(a: A): Unit = {
        var rover = end.prev
        while(rover != end && higherP(a, rover.data)) rover = rover.prev
        rover.next.prev = new Node(a, rover, rover.next)
        rover.next = rover.next.prev 
    }

    def dequeue(): A = {
        val ret = end.next.data
        end.next = end.next.next
        end.next.prev = end
        ret
    }

    def peek: A = end.next.data

    def isEmpty: Boolean = end.next == end
}