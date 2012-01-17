/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import org.scalacheck._
import Prop._
import Gen._
import concurrent.acquiring



object AcquirableSpec extends Properties("Acquirable") {
  
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
    
    @volatile var loop = true
    val nullnode = new Node(0, 0)
    @volatile var current: Node = nullnode
    
    val t = new Thread {
      override def run() {
        for (n <- nodes) {
          current = n
          current = nullnode
          n.dispose()
          current = pool.allocate()
        }
        loop = false
      }
    }
    t.start()
    
    while (loop) {
      val (x, y) = acquiring(current) {
        n =>
        n.x = n.y
        (n.x, n.y)
      }
      assert(x == y, x + ", " + y)
    }
    
    t.join()
    
    true :| "acquiring"
  }
  
  property("concurrent.acquiring") = forAll (ranges) {
    range =>
    val pool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.unrolledPool(new Node(-1, 1)) {
        n =>
        n.x = 0
        n.y = 0
      }
    }
    try testAcquiring(range, pool)
    catch {
      case e =>
        Console.err.println(e)
        e.printStackTrace()
        false :| "acquiring"
    }
  }
  
}
