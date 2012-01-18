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
  
  def main(args: Array[String]) {
    val times = if (args.length == 0) 1 else args(0).toInt
    var measurements: List[Long] = Nil
    for (i <- 0 until times) {
      val start = Platform.currentTime
      run()
      val end = Platform.currentTime
      
      val time = (end - start)
      println(i + ") Running time: " + time + " ms")
      measurements ::= time
    }
    println(">>>")
    println(">>> All running times: " + measurements.reverse.mkString("\t"))
    println(">>>")
  }
  
  def run(): Unit
  
}


object MultiHeap extends MultiConfig {
  
  def run() {
    val sz = size / par
    
    val threads = for (_ <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        var foo: Foo = null
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


/** Parallelization terrible - slowing down for 2 cores already. */
object MultiThreadLocalExperiment extends MultiConfig {
  
  final class Reader(val sz: Int, val tl: ThreadLocal[Foo]) extends Thread {
    override def run() {
      var i = 0
      var foo: Foo = null
      while (i < sz) {
        foo = tl.get
        foo.x = 1
        i += 1
      }
    }
  }
  
  def run() {
    val sz = size / par
    val threadlocal = new ThreadLocal[Foo] {
      override def initialValue = new Foo
    }
    
    val threads = for (_ <- 0 until par) yield new Reader(sz, threadlocal)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


/** Parallelization almost linear up to 8 cores. */
object MultiVolatileExperiment extends MultiConfig {
  
  final class Reader(val sz: Int, idx: Int) extends Thread {
    @volatile var vfoo = new Foo
    override def run() {
      var i = 0
      while (i < sz) {
        vfoo.x = 1
        i += 1
      }
    }
  }
  
  def run() {
    val sz = size / par
    val threads = for (i <- 0 until par) yield new Reader(sz, i)
    
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
        var foo: Foo = null
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


