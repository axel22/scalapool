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
          val foo = table((i + j * 4096) % table.length)
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


abstract class ProducerConsumer extends MultiMain {
  
  class Node {
    @volatile var elem = -1
    @volatile var data = 0.0
    @volatile var next: Node = null
  }
  
  def allocate(): Node
  
  def dispose(n: Node): Unit
  
  def produce(n: Node, i: Int): Unit
  
  def consume(n: Node, i: Int): Unit
  
  class Producer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        val next = allocate()
        produce(next, i)
        channel.next = next
        channel = next
        i += 1
      }
    }
  }
  
  class Consumer(idx: Int, sz: Int, var channelStart: Node) extends Thread {
    override def run() {
      var channel = channelStart
      channelStart = null
      
      var i = 0
      while (i < sz) {
        consume(channel, i)
        while ((channel.next eq null) && (i < sz)) Thread.`yield` // Thread.sleep(0, 200)
        
        val old = channel
        channel = channel.next
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
  
}


trait CheapPC extends ProducerConsumer {
  
  def produce(n: Node, i: Int) {
    n.elem = i
  }
  
  def consume(n: Node, i: Int) {
    assert(i == n.elem + 1)
    n.elem = -n.elem
  }
  
}


abstract class HeapProducerConsumer extends ProducerConsumer {
  
  def allocate() = new Node
  
  def dispose(n: Node) {}
  
}


abstract class CPoolProducerConsumer extends ProducerConsumer {
  
  val pool = new CPool[Node](par)(new Node)(null)
  
  def allocate() = pool.allocate()
  
  def dispose(old: Node) {
    old.next = null
    pool.dispose(old)
  }
  
  override def onExit() {
    pool.printState()
  }
  
}


object CheapHeapPC extends HeapProducerConsumer with CheapPC


object CheapCPoolPC extends CPoolProducerConsumer with CheapPC


trait ExpensivePC extends ProducerConsumer {
  
  def produce(n: Node, i: Int) {
    n.elem = math.cos(math.sqrt(i)).toInt
  }
  
  def consume(n: Node, i: Int) {
    val x = math.sqrt(n.elem)
    val y = math.sqrt(x)
    n.elem = math.atan(y).toInt
  }
  
}


object ExpensiveHeapPC extends HeapProducerConsumer with ExpensivePC


object ExpensiveCPoolPC extends CPoolProducerConsumer with ExpensivePC


abstract class BinaryTree extends MultiMain {
self =>
  
  def allocate(): Node
  
  def dispose(n: Node): Unit
  
  trait Tree {
    def insert(x: Int): Tree
    def remove(x: Int): Tree
    def balanceFactor: Int
    def depth: Int
    def stringRep: String
    def dispose(): Unit
    
    def asNode = this.asInstanceOf[Node]
    def isNode = false
    override def toString = toString(0)
    def toString(lev: Int) = " " * lev + stringRep
  }
  
  case object Empty extends Tree {
    def insert(x: Int) = {
      val n = allocate()
      n.elem = x
      n.depth = 1
      assert(n.left == Empty && n.right == Empty, n)
      n
    }
    def remove(x: Int) = this
    def balanceFactor = 0
    def depth = 0
    def stringRep = "Empty\n"
    def dispose() {}
  }
  
  class Node extends Tree {
    var elem: Int = 0
    var depth: Int = 0
    @volatile var left: Tree = Empty
    @volatile var right: Tree = Empty
    
    override def isNode = true
    
    def insert(x: Int): Tree = {
      if (x == elem) this
      else if (x < elem) {
        left = left.insert(x)
        updateDepth()
        balance()
      } else {
        right = right.insert(x)
        updateDepth()
        balance()
      }
    }
    
    def balanceFactor = left.depth - right.depth
    
    def balance(): Node = {
      val bf = left.depth - right.depth
      if (bf >= -1 && bf <= 1) this
      else if (bf == -2) {
        if (right.balanceFactor == +1) right = right.asNode.rotateRight()
        rotateLeft()
      } else if (bf == +2) {
        if (right.balanceFactor == -1) left = left.asNode.rotateLeft()
        rotateRight()
      } else sys.error("not possible")
    }
    
    def rotateLeft(): Node = {
      val root = right.asNode
      this.right = root.right
      root.left = this
      this.updateDepth()
      root.updateDepth()
      root
    }
    
    def rotateRight(): Node = {
      val root = left.asNode
      this.left = root.right
      root.right = this
      this.updateDepth()
      root.updateDepth()
      root
    }
    
    def updateDepth() {
      depth = math.max(left.depth, right.depth) + 1
    }
    
    def remove(x: Int): Tree = {
      // TODO
      Empty
    }
    
    def dispose() {
      left.dispose()
      right.dispose()
      
      this.left = Empty
      this.right = Empty
      self.dispose(this)
    }
    
    def stringRep = "Node(%d, %d)\n".format(elem, depth)
    
    override def toString(lev: Int) = " " * lev + stringRep + left.toString(lev + 1) + right.toString(lev + 1)
    
  }
  
  class Inserter(val idx: Int, val sz: Int, val repeats: Int) extends Thread {
    override def run() {
      def elem(i: Int, off: Int) = i * 2 + off
      var tree: Tree = Empty
      
      for (round <- 0 until repeats) {
        // fill
        var i = 0
        while (i < sz) {
          val ins = elem(i, 0)
          tree = tree.insert(ins)
          i += 1
        }
        
        // dispose
        tree.dispose()
        
        tree = Empty
      }
    }
  }
  
  def run() {
    val work = size
    val repeats = 1024 / par
    
    val threads = for (index <- 0 until par) yield new Inserter(index, work, repeats)
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
}


object HeapBinaryTree extends BinaryTree {
  
  def allocate() = new Node
  
  def dispose(n: Node) {}
  
}


object CPoolBinaryTree extends BinaryTree {

  val pool = new CPool[Node](par)(new Node)(null)
  
  def allocate() = pool.allocate()
  
  def dispose(old: Node) {
    pool.dispose(old)
  }
  
  override def onExit() {
    pool.printState()
  }
  
}














