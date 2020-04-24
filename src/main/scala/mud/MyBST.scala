package mud

import scala.collection.mutable

class BSTMap[K, V](lt: (K, K) => Boolean) extends mutable.Map[K, V] {
  import BSTMap.Node
  private var root: Node[K, V] = null

  def get(key: K): Option[V] = {
    var rover = root
    while (rover != null && rover.kv._1 != key) {
      if (lt(key, rover.kv._1)) rover = rover.left
      else rover = rover.right
    }
    if (rover == null) None else Some(rover.kv._2)
    // Option(rover).map(_._2)
  }

  def iterator: Iterator[(K, V)] = ???

  def += (kv: (K, V)) = {
    def helper(n: Node[K, V]): Node[K, V] = {
      if (n == null) {
        new Node(kv, null, null)
      } else {
        if (kv._1 == n.kv._1) {
          n.kv = kv
        } else if (lt(kv._1, n.kv._1)) {
          n.left = helper(n.left)
        } else {
          n.right = helper(n.right)
        }
        n
      }
    }
    root = helper(root)
    this
  }

  def -= (key: K) = {
    ???
    this
  }

}

object BSTMap {
  private class Node[K, V](var kv: (K, V), var left: Node[K, V], var right: Node[K, V])
}