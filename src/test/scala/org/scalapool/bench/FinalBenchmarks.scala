/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package bench



import concurrent.CPool



object MultiHeapBurst extends MultiMain {
  
  val burst = System.getProperty("burst").toInt

  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val burstarray = new Array[Foo](burst)
      while (i < sz) {
        var j = 0
        while (j < burst) {
          val foo = new Foo
          foo.x = 1
          burstarray(j) = foo
          j += 1
        }
        
        j = 0
        while (j < burst) {
          val foo = burstarray(j)
          j += 1
        }
        
        i += burst
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new StackThread(index, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiCPoolBurst extends MultiMain {
  
  val burst = System.getProperty("burst").toInt
  val pool = new CPool[Foo](par)(new Foo)(null)
  
  
  import pool._
  
  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val burstarray = new Array[Foo](burst)
      while (i < sz) {
        var j = 0
        while (j < burst) {
          val foo = allocate()
          foo.x = 1
          burstarray(j) = foo
          j += 1
        }
        
        j = 0
        while (j < burst) {
          val foo = burstarray(j)
          dispose(foo)
          j += 1
        }
        
        i += burst
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new StackThread(index, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  override def onExit() {
    printState()
  }
  
}


object MultiHeap extends MultiMain {
  
  class HeapThread(sz: Int) extends Thread {
    var f: Foo = null
    override def run() {
      var i = 0
      while (i < sz) {
        val foo = new Foo
        foo.x = i
        if (i % 442 == 0) f = foo
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


object MultiCPool extends MultiMain {
  
  val pool = new CPool[Foo](par)(new Foo)(null)
  
  import pool._
  
  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val idx = index
      while (i < sz) {
        val foo = allocate()
        foo.x = 1
        dispose(foo)
        i += 1
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new StackThread(index, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  override def onExit() {
    printState()
  }
  
}


object MultiHeapHashtable extends MultiMain {
  
  val burst = System.getProperty("burst").toInt

  class StackThread(index: Int, sz: Int) extends Thread {
    val table = new Array[Foo](1000001)
    override def run() {
      var i = 0
      val burstarray = new Array[Foo](burst)
      while (i < sz) {
        var j = 0
        while (j < burst) {
          val foo = new Foo
          foo.x = 1
          table((i + j * 4096) % table.length) = foo
          j += 1
        }
        
        j = 0
        while (j < burst) {
          val foo = table((i + j) % table.length)
          table((i + j * 4096) % table.length) = null
          j += 1
        }
        
        i += burst
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new StackThread(index, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object MultiCPoolHashtable extends MultiMain {
  
  val burst = System.getProperty("burst").toInt
  val pool = new CPool[Foo](par)(new Foo)(null)
  
  import pool._
  
  class StackThread(index: Int, sz: Int) extends Thread {
    val table = new Array[Foo](1000001)
    override def run() {
      var i = 0
      while (i < sz) {
        var j = 0
        while (j < burst) {
          val foo = allocate()
          foo.x = 1
          table((i + j * 4096) % table.length) = foo
          j += 1
        }
        
        j = 0
        while (j < burst) {
          val foo = table((i + j * 4096) % table.length)
          dispose(foo)
          table((i + j * 4096) % table.length) = null
          j += 1
        }
        
        i += burst
      }
    }
  }
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new StackThread(index, sz)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  override def onExit() {
    printState()
  }
  
}


object HeapProducerConsumer extends MultiMain {
  
  class Node {
    @volatile var elem = -1
    @volatile var next: Node = null
  }
  
  class Producer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        val next = new Node
        next.elem = i
        channel.next = next
        channel = next
        i += 1
      }
    }
  }
  
  class Consumer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    var local: Int = 0
    
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        local = channel.elem
        assert(i == local + 1)
        local = math.sqrt(local).toInt // compute a bit
        while ((channel.next eq null) && (i < sz)) Thread.sleep(0, 1000)
        channel = channel.next
        i += 1
      }
    }
  }
  
  def run() {
    val numpairs = par / 2
    val sz = size / numpairs
    
    val pairs = for (index <- 0 until numpairs) yield {
      val channel = new Node
      (new Producer(index, sz, channel), new Consumer(index, sz, channel))
    }
    
    pairs.foreach {
      case (p, c) => 
        p.start()
        c.start()
    }
    pairs.foreach {
      case (p, c) =>
        p.join()
        c.join()
    }
  }
  
}


object CPoolProducerConsumer extends MultiMain {
  
  val pool = new CPool[Node](par)(new Node)(null)
  
  import pool._
  
  class Node {
    @volatile var elem = -1
    @volatile var next: Node = null
  }
  
  class Producer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        val next = allocate()
        next.elem = i
        next.next = null
        
        channel.next = next
        channel = next
        
        i += 1
      }
    }
  }
  
  class Consumer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    var local: Int = 0
    
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        local = channel.elem
        assert(i == local + 1)
        local = math.sqrt(local).toInt // compute a bit
        while ((channel.next eq null) && (i < sz)) Thread.sleep(0, 1000)
        
        val old = channel
        channel = old.next
        old.next = null
        dispose(old)
        
        i += 1
      }
    }
  }
  
  def run() {
    val numpairs = par / 2
    val sz = size / numpairs
    
    val pairs = for (index <- 0 until numpairs) yield {
      val channel = new Node
      (new Producer(index, sz, channel), new Consumer(index, sz, channel))
    }
    
    pairs.foreach {
      case (p, c) => 
        p.start()
        c.start()
    }
    pairs.foreach {
      case (p, c) =>
        p.join()
        c.join()
    }
  }
  
  override def onExit() {
    printState()
  }
  
}









