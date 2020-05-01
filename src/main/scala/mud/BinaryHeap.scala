package mud

import scala.reflect.ClassTag

// import priorityqueues.adt.PriorityQueue

class BinaryHeap[A: ClassTag](comp: (A, A) => Boolean) extends MyPriorityQueue[A] {
    private var heap = new Array[A](10)
    private var back = 1

    def enqueue(obj: A): Unit = {
        if (back >= heap.length) {
            val temp = new Array[A](heap.length * 2)
            Array.copy(heap, 0, temp, 0, heap.length)
            heap = temp
        }
        var bubble = back
        while (bubble > 1 && comp(obj, heap(bubble / 2))) {
            heap(bubble) = heap(bubble / 2)
            bubble /= 2
        }
        heap(bubble) = obj
        back += 1
    }

    def dequeue(): A = {
        val ret = heap(1)
        back -= 1
        val temp = heap(back)
        heap(back) = heap(0)
        var stone = 1
        var flag = true
        while (flag && stone * 2 < back) {
            var priorityChild = stone*2
            if (stone * 2 + 1 < back && comp(heap(stone * 2 + 1), heap(stone * 2))) priorityChild += 1
            if(comp(heap(priorityChild), temp)){
                heap(stone) = heap(priorityChild)
                stone = priorityChild
            }else flag = false
        }
        heap(stone) = temp
        ret
    }

    def peek: A = heap(1)

    def isEmpty: Boolean = back == 1
}