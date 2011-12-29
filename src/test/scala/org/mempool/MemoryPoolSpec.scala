/*   ______          _            ___                       *\
**  /_  __/__ ______(_)___ ____  |_  |                      **
**   / /  / // / __/ // _ `/ _ \/ __/                       **
**  /_/   \_, /_/ /_/ \_,_/_//_/____/     Tyrian 2          **
**       /___/                            www.tyrian2.com   **
\*                                        (c) 2011-2012     */

package org.mempool



import org.scalacheck._
import Prop._
import Gen._



object MemoryPoolSpec extends Properties("MemoryPool") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  class Node(var x: Int) extends Reclaimable
  
  def testAllocate(range: Range, pool: MemoryPool) = {
    pool.register(new Node(-1)) {
      _.x = 0
    }
    
    val nodes = for (n <- range) yield {
      val node = pool.allocate[Node]
      node.x = n
      node
    }
    
    (range zip nodes.map(_.x)).forall {
      case (x, y) => x == y
    } :| "nodes have growing `x`: %s"
  }
  
  property("SingleThreadUnlimitedPool.allocate") = forAll (ranges) {
    range =>
    val pool = new SingleThreadUnlimitedPool
    testAllocate(range, pool)
  }
  
  property("SingleThreadFixedPool.allocate") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = new SingleThreadFixedPool(capacity)
    testAllocate(range, pool)
  }
  
  def testReclaim(range: Range, pool: MemoryPool): Prop = {
    pool.register(new Node(-1)) { _.x = 0 }
    
    for (n <- range) {
      val node = pool.allocate[Node]
      node.x = n
      node.dispose()
    }
    
    val nodes = for (n <- range) yield pool.allocate[Node]
    
    nodes.forall(_.x == 0)
  }
  
  property("SingleThreadUnlimitedPool.reclaim") = forAll (ranges) {
    range =>
    val pool = new SingleThreadUnlimitedPool
    testReclaim(range, pool)
  }
  
  property("SingleThreadFixedPool.reclaim") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = new SingleThreadFixedPool(capacity)
    testReclaim(range, pool)
  }
  
}
