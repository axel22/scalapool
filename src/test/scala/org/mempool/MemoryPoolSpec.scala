/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import org.scalacheck._
import Prop._
import Gen._



object MemoryPoolSpec extends Properties("MemoryPool") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  class Node(var x: Int) extends singlethread.Poolable[Node] with singlethread.Linkable[Node]
  
  def testAllocate(range: Range, pool: MemoryPool[Node]) = {
    val nodes = for (n <- range) yield {
      val node = pool.allocate()
      node.x = n
      node
    }
    
    (range zip nodes.map(_.x)).forall {
      case (x, y) => x == y
    } :| "nodes have growing `x`: %s"
  }
  
  property("singlethread.UnlimitedPool.allocate") = forAll (ranges) {
    range =>
    val pool = Allocator.singleThread.unlimitedPool(new Node(-1)) {
      _.x = 0
    }
    testAllocate(range, pool)
  }
  
  property("singlethread.FixedPool.allocate") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.fixedPool(capacity)(new Node(-1)) {
      _.x = 0
    }
    testAllocate(range, pool)
  }
  
  property("singlethread.GrowingPool.allocate") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.growingPool(new Node(-1)) {
      _.x = 0
    }
    testAllocate(range, pool)
  }
  
  property("singlethread.FreeList.allocate") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.freeList(new Node(-1)) {
      _.x = 0
    }
    testAllocate(range, pool)
  }
  
  def testDispose(range: Range, pool: MemoryPool[Node]): Prop = {
    for (n <- range) {
      val node = pool.allocate()
      node.x = n
      node.dispose()
    }
    
    val nodes = for (n <- range) yield pool.allocate()
    
    nodes.forall(_.x == 0)
  }
  
  property("singlethread.UnlimitedPool.dispose") = forAll (ranges) {
    range =>
    val pool = Allocator.singleThread.unlimitedPool(new Node(-1)) { _.x = 0 }
    testDispose(range, pool)
  }
  
  property("singlethread.FixedPool.dispose") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.fixedPool(capacity)(new Node(-1)) { _.x = 0 }
    testDispose(range, pool)
  }
  
  property("singlethread.GrowingPool.dispose") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.growingPool(new Node(-1)) { _.x = 0 }
    testDispose(range, pool)
  }
  
  property("singlethread.FreeList.dispose") = forAll (ranges, choose(0, 8)) {
    (range, capacity) =>
    val pool = Allocator.singleThread.freeList(new Node(-1)) { _.x = 0 }
    testDispose(range, pool)
  }
  
}
