/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package util






/** Unrolled stack.
 */
class UnrolledStack[T: ClassManifest] extends Stack[T] {
  import UnrolledStack.Node
  
  val allocator: Allocator[Node[T]] = Allocator.singleThread.fixedPool(16)(new Node[T](null)) { _.prev = null }
  var stacklet = new Node(null)
  
  def push(x: T) = stacklet = stacklet.push(x, this)
  
  def pop() = stacklet.pop(this)
  
  override def isEmpty = stacklet.isEmpty && !stacklet.hasPredecessor
  
  def isFull = false
  
  def foreach[U](f: T => U) = stacklet.foreach(f)
  
  override def toString = "UnrolledStack[top stacklet: %s](%s)".format(
    stacklet.array.take(stacklet.front).mkString(", "),
    this.mkString(", ")
  )
  
}


object UnrolledStack {
  
  val NODESIZE = 64
  
  final class Node[T: ClassManifest](var prev: Node[T]) {
    val array = new Array[T](NODESIZE)
    var front = 0
    
    def isEmpty = front == 0
    
    def hasPredecessor = prev ne null
    
    def push(x: T, us: UnrolledStack[T]): Node[T] = if (front < NODESIZE) {
      array(front) = x
      front += 1
      this
    } else {
      val n = us.allocator.allocate()
      n.prev = this
      n.push(x, us)
    }
    
    def pop(us: UnrolledStack[T]): T = if (!isEmpty) {
      front -= 1
      val elem = array(front)
      array(front) = null.asInstanceOf[T]
      elem
    } else {
      us.stacklet = prev
      us.allocator.dispose(this)
      prev.pop(us)
    }
    
    def foreach[U](f: T => U) {
      var i = front
      while (i > 0) {
        i -= 1
        f(array(i))
      }
      if (hasPredecessor) prev.foreach(f)
    }
    
  }
  
}



