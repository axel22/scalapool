/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package util



import singlethread.Poolable



/** Unrolled stack.
 */
final class UnrolledStack[T >: Null <: AnyRef: ClassManifest] extends Stack[T] {
  import UnrolledStack.{Node, NODESIZE}
  
  private val allocator: Allocator[Node[T]] = Allocator.singleThread.fixedPool(new Node[T](null))(16) { _.prev = null }
  private var stacklet = new Node(null)
  
  final def push(x: T): Unit = {
    val curr = stacklet
    if (curr.front < NODESIZE) {
      curr.array(curr.front) = x
      curr.front += 1
    } else {
      val n = allocator.allocate()
      n.prev = curr
      n.array(0) = x
      n.front = 1
      stacklet = n
    }
  }
  
  final def pop(): T = {
    val curr = stacklet
    if (curr.front > 0) {
      curr.front -= 1
      val elem = curr.array(curr.front)
      curr.array(curr.front) = null
      elem
    } else bigPop()
  }
  
  private def bigPop(): T = {
    val curr = stacklet
    if (curr.prev ne null) {
      val p = curr.prev
      stacklet = p
      allocator.dispose(curr)
      
      p.front -= 1
      val elem = p.array(p.front)
      p.array(p.front) = null
      elem
    } else null
  }
  
  final override def isEmpty = stacklet.isEmpty && !stacklet.hasPredecessor
  
  def isFull = false
  
  def foreach[U](f: T => U) = stacklet.foreach(f)
  
  override def toString = "UnrolledStack[top stacklet: %s](%s)".format(
    stacklet.string,
    this.mkString(", ")
  )
  
}


object UnrolledStack {
  
  val NODESIZE = 64
  
  final class Node[T: ClassManifest](var prev: Node[T]) extends Poolable[Node[T]] {
    val array = new Array[T](NODESIZE)
    final var front = 0
    
    private[scalapool] def string = array.take(front).mkString(", ")
    
    def isEmpty = front == 0
    
    def hasPredecessor = prev ne null
    
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



