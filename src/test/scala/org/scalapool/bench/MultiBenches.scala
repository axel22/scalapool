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



trait MultiConfig {
  val size = System.getProperty("size").toInt
  val par = System.getProperty("par").toInt
  var foo: Foo = null
  
  def main(args: Array[String]) {
    val start = Platform.currentTime
    run()
    val end = Platform.currentTime
    
    println("Running time: " + (end - start) + " ms")
  }
  
  def run(): Unit
  
}


object MultiHeap extends MultiConfig {
  
  def run() {
    val sz = size / par
    
    val threads = for (_ <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        while (i < sz) {
          foo = new Foo
          foo.x = 1
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiThreadLocalExperiment extends MultiConfig {
  
  def run() {
    val sz = size / par
    val mempool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.freeList(new Foo) {
        _.x = 0
      }
    }
    
    val threadlocal = new ThreadLocal[Foo] {
      override def initialValue = new Foo
    }
    
    val threads = for (_ <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        while (i < sz) {
          foo = threadlocal.get
          foo.x = 1
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiThreadLocalFreeList extends MultiConfig {
  
  def run() {
    val sz = size / par
    val mempool = Allocator.concurrent.threadLocalPool {
      Allocator.singleThread.freeList(new Foo) {
        _.x = 0
      }
    }
    
    val threads = for (_ <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        while (i < sz) {
          foo = mempool.allocate()
          foo.x = 1
          mempool.dispose(foo)
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


