/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import org.scalacheck._
import Prop._
import Gen._



object ConcurrentMemoryPoolSpec extends Properties("ConcurrentMemoryPool") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  class Node(var x: Int) extends concurrent.Poolable[Node]
  
  def testAllocate(range: Range, pool: ConcurrentMemoryPool[Node]) = {
    val nodes = for (n <- range.par) yield {
      val node = pool.allocate()
      node.x = n
      node
    }
    
    (range zip nodes.map(_.x)).forall {
      case (x, y) => x == y
    } :| "nodes have growing `x`: %s"
  }
  
  property("concurrent.ThreadLocalPool.allocate") = forAll (ranges) {
    range =>
    val pool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.unlimitedPool(new Node(-1)) { _.x = 0 }
    }
    testAllocate(range, pool)
  }
  
  def testDispose(range: Range, pool: ConcurrentMemoryPool[Node]): Prop = {
    for (n <- range.par) {
      val node = pool.allocate()
      node.x = n
      node.dispose()
    }
    
    val nodes = for (n <- range) yield pool.allocate()
    
    nodes.forall(_.x == 0)
  }
  
  property("concurrent.ThreadLocalPool.dispose") = forAll (ranges) {
    range =>
    val pool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.unlimitedPool(new Node(-1)) { _.x = 0 }
    }
    testDispose(range, pool)
  }
  
}
