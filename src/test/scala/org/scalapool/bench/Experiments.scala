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
import annotation.tailrec



object OneObject extends MultiMain {
  
  var reference: Foo = null
  
  def run() {
    val sz = size / par
    
    val threads = for (i <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        while (i < sz) {
          val foo = allocate()
          foo.x = 1
          dispose(foo)
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  // pool
  
  var obj: Foo = new Foo
  
  def allocate(): Foo = {
    obj
  }
  
  def dispose(f: Foo) {
    obj = f
  }
  
}


/** This one is difficult to explain.
 *  
 *  Sometimes it scales well, sometimes not. Decreasing STRIDE to 128 results in no scalability
 *  whatsoever. Setting STRIDE to 32167 results in nice scalability almost always.
 *  
 *  Now comes the strange part - for STRIDE 128, removing either the:
 *  
 *    foo.x = 1
 *  
 *  or:
 *  
 *    dispose(idx, foo)
 *  
 *  results in almost linear scalability. The `either` part is hard to explain.
 *  
 *  Note that the tests _were_ run with conditional card marking.
 */
object ObjectArray extends MultiMain {
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new Thread {
      override def run() {
        var i = 0
        val idx = index
        while (i < sz) {
          val foo = allocate(idx)
          foo.x = 1
          dispose(idx, foo)
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  // pool
  
  final def STRIDE = 16384
  
  val arr = Array.fill[Foo](par * STRIDE)(new Foo)
  
  def allocate(idx: Int): Foo = {
    arr(idx * STRIDE)
  }
  
  def dispose(idx: Int, f: Foo) {
    arr(idx * STRIDE) = f
  }
  
}


/** Similar effects as before... */
object ObjectStacks extends MultiMain {
  
  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val idx = index
      while (i < sz) {
        val foo = allocate(idx)
        foo.x = 1
        dispose(idx, foo)
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
  
  // pool
  
  final def SIZE = 32168
  
  // layout of the objects here DOES matter
  val arr = new Array[Array[Foo]](par)
  for (i <- 0 until par) {
    arr(i) = new Array[Foo](SIZE)
    for (j <- 0 until SIZE) arr(i)(j) = new Foo
  }
  
  def allocate(idx: Int): Foo = {
    val stack = arr(idx)
    stack(0)
  }
  
  def dispose(idx: Int, f: Foo) {
    val stack = arr(idx)
    stack(0) = f
  }
  
}


object ObjectPosStacks extends MultiMain {
  
  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val idx = index
      while (i < sz) {
        val foo = allocate(idx)
        foo.x = 1
        dispose(idx, foo)
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
  
  // pool
  
  final def SIZE = 32168
  
  // layout of the objects here DOES matter
  val pos = new Array[Int](par)
  for (i <- 0 until par) pos(i) = i * (SIZE / 4 / par)
  val arr = new Array[Array[Foo]](par)
  for (i <- 0 until par) {
    arr(i) = new Array[Foo](SIZE)
    for (j <- 0 until SIZE) arr(i)(j) = new Foo
  }
  
  def allocate(idx: Int): Foo = {
    val stack = arr(idx)
    val p = pos(idx)
    stack(p)
  }
  
  def dispose(idx: Int, f: Foo) {
    val stack = arr(idx)
    val p = pos(idx)
    stack(p) = f
  }
  
}


/** This experiment reveals that a CAS might be too expensive, although scalable. */
object CasStacks extends MultiMain {
  
  class StackThread(index: Int, sz: Int) extends Thread {
    override def run() {
      var i = 0
      val idx = index
      while (i < sz) {
        val foo = allocate(idx)
        foo.x = 1
        dispose(idx, foo)
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
  
  // pool
  
  import java.util.concurrent.atomic.AtomicReferenceArray
  
  final def SIZE = 32168
  
  // layout of the objects here DOES matter
  val arr = new Array[AtomicReferenceArray[Foo]](par)
  for (i <- 0 until par) {
    arr(i) = new AtomicReferenceArray[Foo](SIZE)
    for (j <- 0 until SIZE) arr(i).set(j, new Foo)
  }
  
  def allocate(idx: Int): Foo = {
    val stack = arr(idx)
    val v = stack.get(0)
    stack.compareAndSet(0, v, null)
    v
  }
  
  def dispose(idx: Int, f: Foo) {
    val stack = arr(idx)
    stack.compareAndSet(0, null, f)
  }
  
}


object HashStacks extends MultiMain {
  
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
  
  // pool
  
  final def SIZE = 32168
  
  // layout of the objects here DOES matter
  val arr = new Array[Array[Foo]](par * 9)
  
  def init(idx: Int) {
    arr(idx) = new Array[Foo](SIZE)
    for (j <- 0 until SIZE) arr(idx)(j) = new Foo
  }
  
  def allocate(): Foo = {
    val tid = Thread.currentThread.getId
    val idx = tid.toInt % arr.length
    val stack = arr(idx)
    if (stack ne null) stack(0)
    else {
      init(idx)
      arr(idx)(0)
    }
  }
  
  def dispose(f: Foo) {
    val tid = Thread.currentThread.getId
    val idx = tid.toInt % arr.length
    val stack = arr(idx)
    stack(0) = f
  }
  
}


/** Performance characteristics not good. Moreover, this may leak objects causing GC. */
object HashFreeList extends MultiMain {
  
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
  
  // pool
  
  import java.util.concurrent.atomic.AtomicReferenceArray
  
  val arr = new AtomicReferenceArray[Bar](par * 2048)
  
  @tailrec def allocate(): Bar = {
    val tid = Thread.currentThread.getId * 0x9e3775cd
    val idx = math.abs(tid.toInt) % arr.length
    val res = arr.get(idx)
    if (res ne null) {
      if (arr.compareAndSet(idx, res, res._linkable_next)) res
      else allocate()
    } else new Bar
  }
  
  @tailrec def dispose(f: Bar) {
    val tid = Thread.currentThread.getId * 0x9e3775cd
    val idx = math.abs(tid.toInt) % arr.length
    val old = arr.get(idx)
    f._linkable_next = old
    if (arr.compareAndSet(idx, old, f)) {}
    else dispose(f)
  }
  
}


object HashDescriptors extends MultiMain {
  
  val pool = new CPool(par)
  
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
  
}


object HashDescriptorsBurst extends MultiMain {
  
  val burst = System.getProperty("burst").toInt
  val pool = new CPool(par)
  
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
  
}


final class CPool(val par: Int) {
  
  import java.util.concurrent.atomic._
  
  final def BLOCKSIZE = 256
  final def TIMESTAMP_OFFSET = 22
  final def TIMESTAMP_GROWTH = 1 << 22
  final def TIMESTAMP_MASK = ((1 << 42) - 1) << TIMESTAMP_OFFSET
  final def INDEX_MASK = ((1 << 22) - 1)
  
  final class Descriptor(val tid: Long) {
    var active = new Block()
    var top = active
    var activeArray = active.array
    var activePos: Int = 128
  }
  
  final class Block {
    val array = new Array[Foo](BLOCKSIZE)
    @volatile var idx: Int = 0
    @volatile var next: Block = null
  }
  
  // descriptors is deliberately not an AtomicRefArray
  // 'cause we can live with a weak get
  val descs = new Array[Descriptor](par * 127)
  val freepool = new AtomicLongArray(par)
  val blockcounter = new AtomicInteger(1)
  @volatile var blockarray = new AtomicReferenceArray[Block](par * 32)
  
  @inline def descriptor(): Descriptor = {
    val tid = Thread.currentThread.getId
    var hashed = tid.toInt
    if (hashed < 0) hashed = -hashed
    val idx = hashed % descs.length
    val d = descs(idx)
    if ((d ne null) && d.tid == tid) d
    else findDescriptor(tid, idx)
  }
  
  def findDescriptor(tid: Long, start: Int): Descriptor = {
    var idx = start
    while (true) {
      val d = descs(idx)
      if (d ne null) {
        if (d.tid == tid) return d
        else idx = (idx + 1) % descs.length
      } else {
        val d = new Descriptor(tid)
        halfFill(d.active)
        descs(idx) = d
      }
    }
    sys.error("unreachable code")
  }
  
  def allocate(): Foo = {
    val d = descriptor()
    var pos = d.activePos
    if (pos > 0) {
      pos -= 1
      d.activePos = pos
      d.activeArray(pos)
    } else {
      decBlock(d)
      allocate()
    }
  }
  
  def dispose(f: Foo) {
    val d = descriptor()
    var pos = d.activePos
    if (pos < BLOCKSIZE) {
      d.activeArray(pos) = f
      d.activePos = pos + 1
    } else {
      incBlock(d)
      dispose(f)
    }
  }
  
  def decBlock(d: Descriptor) {
    if (d.top ne d.active) {
      // previous block => top != active
      d.active = d.top
      d.activeArray = d.active.array
      d.activePos = BLOCKSIZE
    } else {
      // no previous block => top == active
      var block = stealBlock()
      if (block eq null) block = allocateFullBlock()
      setNext(block, d.top)
      d.top = block
      d.active = block
      d.activeArray = d.active.array
      d.activePos = BLOCKSIZE
    }
  }
  
  def incBlock(d: Descriptor) {
    var block = getNext(d.active)
    if (block eq null) {
      block = allocateEmptyBlock()
      setNext(d.active, block)
    }
    if (d.active eq d.top) {
      d.active = block
      d.activeArray = d.active.array
      d.activePos = 0
    } else {
      releaseBlock(d.top)
      d.top = d.active
      d.active = block
      d.activeArray = d.active.array
      d.activePos = 0
    }
  }
  
  def getNext(b: Block): Block = {
    b.next
  }
  
  def setNext(b: Block, next: Block) {
    b.next = next
  }
  
  def stealBlock(): Block = {
    // do one round on the freepool and try to steal
    var i = 0
    while (i < par) {
      val b = stealAt(i)
      if (b ne null) return b
      i += 1
    }
    
    null
  }
  
  def stealAt(pos: Int): Block = {
    val blockptr = freepool.get(pos)
    if (blockptr == 0) null
    else {
      val stamp = blockptr & TIMESTAMP_MASK
      val nstamp = stamp + TIMESTAMP_GROWTH
      val idx = blockptr & INDEX_MASK
      val block = blockarray.get(idx.toInt)
      val nblock = block.next
      val nidx = if (nblock eq null) 0L else nblock.idx.toLong & ((1L << 32) - 1)
      val nblockptr = nstamp | nidx
      if (freepool.compareAndSet(pos, blockptr, nblockptr)) block
      else null
    }
  }
  
  def releaseBlock(b: Block) {
    // loop until you manage to return the block
    var i = 0
    while (true) {
      if (releaseAt(i, b)) return
      i = (i + 1) % par
    }
  }
  
  def releaseAt(pos: Int, block: Block): Boolean = {
    val oblockptr = freepool.get(pos)
    val ostamp = oblockptr & TIMESTAMP_MASK
    val nstamp = ostamp + TIMESTAMP_GROWTH
    val oidx = oblockptr & INDEX_MASK
    val oblock = if (oidx == 0) null else blockarray.get(oidx.toInt)
    val nidx = block.idx.toLong & ((1L << 32) - 1)
    val nblockptr = nstamp | nidx
    block.next = oblock
    freepool.compareAndSet(pos, oblockptr, nblockptr)
  }
  
  @tailrec
  private def allocblock(b: Block, startsearch: Int) {
    // TODO implement table resize when full
    var idx = startsearch
    if (blockarray.compareAndSet(idx, null, b)) {
      // success - set index for the block
      b.idx = idx
      
      // update block counter to highest value
      var blkcnt: Int = -1
      do {
        blkcnt = blockcounter.get
      } while (blkcnt <= idx && !blockcounter.compareAndSet(blkcnt, idx + 1))
    } else {
      // find next free entry
      while (blockarray.get(idx) != null) idx += 1
      allocblock(b, idx)
    }
  }
  
  def allocateEmptyBlock(): Block = {
    val idx = blockcounter.get
    val block = new Block
    allocblock(block, idx)
    block
  }
  
  def allocateFullBlock(): Block = {
    val b = allocateEmptyBlock()
    fill(b)
    b
  }
  
  def halfFill(b: Block) {
    val array = b.array
    for (i <- 0 until (BLOCKSIZE / 2)) array(i) = new Foo
  }
  
  def fill(b: Block) {
    val array = b.array
    for (i <- 0 until BLOCKSIZE) array(i) = new Foo
  }
  
}



