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



abstract class MultiConfig {
  val size = System.getProperty("size").toInt
  val par = System.getProperty("par").toInt
  
  def mainMethod(args: Array[String]) {
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


abstract class MultiMain extends MultiConfig {
  
  def main(args: Array[String]) {
    mainMethod(args)
  }
  
}


object MultiHeap extends MultiMain {
  
  @volatile var reference: Foo = null
  
  class HeapThread(sz: Int) extends Thread {
    var foo: Foo = null
    override def run() {
      var i = 0
      while (i < sz) {
        foo = new Foo
        foo.x = 1
        reference = foo
        i += 1
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (_ <- 0 until par) yield new HeapThread(sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiHeapOldRef extends MultiMain {
  
  def run() {
    val sz = size / par
    
    val threads = for (_ <- 0 until par) yield new Thread {
      var foo: Foo = null
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


object MultiThreadLocalExperiment extends MultiMain {
  
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


object MultiVolatileExperiment extends MultiMain {
  
  final class Reader(val sz: Int, idx: Int) extends Thread {
    @volatile var vfoo = new Foo
    var bar: Foo = null
    override def run() {
      var i = 0
      while (i < sz) {
        vfoo.x = 1
        bar = vfoo
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


object MultiVolatileExperiment2 extends MultiMain {
  
  final class Holder {
    var bar: Foo = null
  }
  
  final class Reader(val sz: Int, idx: Int) extends Thread {
    @volatile var vfoo = new Foo
    var holder: Holder = null
    override def run() {
      var i = 0
      holder = new Holder
      while (i < sz) {
        vfoo.x = 1
        holder.bar = vfoo
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


object MultiThreadLocalFreeList extends MultiMain {
  
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


object MultiStackExperiment extends MultiMain {
  
  class Worker(private val idx: Int, private val size: Int) extends Thread {
    val array = new Array[Foo](1024)
    
    override def run() {
      val arr = array
      loop(arr)
    }
    
    def loop(arr: Array[Foo]) {
      var v = new Foo
      val sz = size
      var i = 0
      var pos = 512
      while (i < sz) {
        if (i % 2 == 0) {
          arr(pos) = v
          pos += 1
        } else {
          pos -= 1
          v = arr(pos)
        }
        i += 1
      }
    }
    
  }
  
  def run() {
    val sz = size / par
    val threads = for (i <- 0 until par) yield new Worker(i, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiStackExperiment2 extends MultiMain {
  
  class Worker(private val idx: Int, private val size: Int, val array: Array[Foo]) extends Thread {
    override def run() {
      val arr = array
      loop(arr)
    }
    
    def loop(arr: Array[Foo]) {
      var v = new Foo
      val sz = size
      var i = 0
      var pos = 0 + idx * 2048
      while (i < sz) {
        if (i % 2 == 0) {
          arr(pos) = v
          pos += 1
        } else {
          pos -= 1
          v = arr(pos)
        }
        i += 1
      }
    }
    
  }
  
  def run() {
    val sz = size / par
    val array = new Array[Foo](2 << 16)
    val threads = for (i <- 0 until par) yield new Worker(i, sz, array)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}






















