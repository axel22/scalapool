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



trait BenchConfig {
  val size = 500000000
  var foo: Foo = null
  
  def main(args: Array[String]) {
    val start = Platform.currentTime
    run()
    val end = Platform.currentTime
    
    println("Running time: " + (end - start) + " ms")
  }
  
  def run(): Unit
  
}


object BasicHeap extends BenchConfig {
  
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


object BasicFreeList extends BenchConfig {
  
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


class Foo(var x: Int = 0) extends Linkable[Foo]
