/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package bench



import singlethread._
import scala.testing.Benchmark
import compat.Platform



trait BasicConfig {
  val size = System.getProperty("size").toInt
  var foo: Foo = null
  
  def main(args: Array[String]) {
    val start = Platform.currentTime
    run()
    val end = Platform.currentTime
    
    println("Running time: " + (end - start) + " ms")
  }
  
  def run(): Unit
  
}


object BasicHeap extends BasicConfig {
  
  def run() {
    var i = 0
    val sz = size
    while (i < sz) {
      foo = new Foo
      foo.x = 1
      i += 1
    }
  }
  
}


object BasicHeapAllocator extends BasicConfig {
  
  def run() {
    val mempool = Allocator.heap(new Foo) {
      _.x = 0
    }
    
    var i = 0
    val sz = size
    while (i < sz) {
      foo = mempool.allocate()
      foo.x = 1
      mempool.dispose(foo)
      i += 1
    }
  }
  
}


object BasicFreeList extends BasicConfig {
  
  def run() {
    val mempool = Allocator.singleThread.freeList(new Foo) {
      _.x = 0
    }
    
    var i = 0
    val sz = size
    while (i < sz) {
      foo = mempool.allocate()
      foo.x = 1
      mempool.dispose(foo)
      i += 1
    }
  }
  
}


object BasicUnrolledPool extends BasicConfig {
  
  def run() {
    val mempool = Allocator.singleThread.unrolledPool(new Foo) {
      _.x = 0
    }
    
    var i = 0
    val sz = size
    while (i < sz) {
      foo = mempool.allocate()
      foo.x = 1
      mempool.dispose(foo)
      i += 1
    }
  }
  
}


object BasicFixedPool extends BasicConfig {
  
  def run() {
    val mempool = Allocator.singleThread.fixedPool(new Foo)(16) {
      _.x = 0
    }
    
    var i = 0
    val sz = size
    while (i < sz) {
      foo = mempool.allocate()
      foo.x = 1
      mempool.dispose(foo)
      i += 1
    }
  }
  
}


object BasicGrowingPool extends BasicConfig {
  
  def run() {
    val mempool = Allocator.singleThread.growingPool(new Foo) {
      _.x = 0
    }
    
    var i = 0
    val sz = size
    while (i < sz) {
      foo = mempool.allocate()
      foo.x = 1
      mempool.dispose(foo)
      i += 1
    }
  }
  
}


