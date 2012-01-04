/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import org.scalacheck._
import Prop._
import Gen._
import concurrent.Ref



object RefSpec extends Properties("Ref") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  class Node(var x: Int, var y: Int) extends concurrent.Acquirable[Node]
  
  def testAcquiring(range: Range, pool: ConcurrentMemoryPool[Node]) = {
    val nodes = for (n <- range.par) yield {
      val node = pool.allocate()
      node.x = n
      node.y = -n
      node
    }
    
    val current = Ref[Node]()
    val zeronode = new Node(0, 0)
    
    val t = new Thread {
      override def run() {
        try {
          /*
          for (n <- nodes) {
            current := n
            current := null
            n.dispose()
            val ncurrent = pool.allocate()
            current := ncurrent
          }
          */
        } catch {
          case e => e.printStackTrace()
        } finally {
          current := zeronode
        }
      }
    }
    
    t.start()
    while (current() ne zeronode) {
      val n = current()
      assert((n eq null) || n.x == -n.y, n.x + ", " + n.y)
    }
    t.join()
    
    true
  }
  
  property("<<") = forAll (ranges) {
    range =>
    val pool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.unlimitedPool(new Node(-1, 1)) {
        n =>
        n.x = -1
        n.y = 1
      }
    }
    try { testAcquiring(range, pool) } catch {
      case e => e.printStackTrace(); false
    }
  }
  
}
