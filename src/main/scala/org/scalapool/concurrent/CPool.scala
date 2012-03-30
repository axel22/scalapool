/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package concurrent



import annotation.unchecked._
import annotation.tailrec



/** A concurrent memory pool.
 */
final class CPool[R >: Null <: AnyRef: Manifest](val par: Int, val useCleaner: Boolean = true)(ctor: =>R)(init: R => Unit)
extends ConcurrentMemoryPool[R] {
  
  import java.util.concurrent.atomic._
  
  final def BLOCKSIZE = 256
  final def TIMESTAMP_OFFSET = 22
  final def TIMESTAMP_GROWTH = 1 << 22
  final def TIMESTAMP_MASK = ((1 << 42) - 1) << TIMESTAMP_OFFSET
  final def INDEX_MASK = ((1 << 22) - 1)
  
  final class Descriptor(val tid: Long, val thread: Thread, idx: Int) {
    val descriptorIndex: DescriptorIndex = new DescriptorIndex(tid, idx, this)
    var active = allocateEmptyBlock()
    var top = active
    var activeArray = active.array
    var activePos: Int = active.pos
    
    def isLive = descriptorIndex.lastd ne null
  }
  
  final class Block {
    val array = new Array[R](BLOCKSIZE)
    var pos = 0
    @volatile var idx: Int = 0
    @volatile var next: Block = null
  }
  
  final class DescriptorIndex(val tid: Long, val idx: Int, @volatile var lastd: Descriptor) {
    val lastd_setter = AtomicReferenceFieldUpdater.newUpdater(classOf[DescriptorIndex], classOf[Descriptor], "lastd")
    
    def CAS_lastd(oval: Descriptor, nval: Descriptor): Boolean = 
      lastd_setter.compareAndSet(this, oval, nval)
    
    def release() {
      val d = lastd
      
      if (d eq null) { /* we're done */ }
      else if (CAS_lastd(d, null)) { // ensure only 1 release!
        // return blocks in `d` to freepool
        if (d.active ne d.top) releaseBlock(tid, d.top)
        d.active.pos = d.activePos
        releaseBlock(tid, d.active)
        
        // remove descriptor (very small chance to create garbage)
        descs(idx) = null
        descsize.getAndDecrement()
      } else release()
    }
    
    override def finalize() = release()
  }
  
  /* fields */
  
  val special = resolveInit(ctor)
  
  // descs
  // is deliberately not an AtomicRefArray
  // 'cause we can live with a weak get - which is faster
  @volatile var descs = new Array[Descriptor](par * 32)
  val descsize = new AtomicInteger(0)
  val descindex = new ThreadLocal[DescriptorIndex] {
    override def initialValue = null
  }
  
  // pool of free object blocks
  val freepool = new AtomicLongArray(par)
  
  // blocks array
  val blockcounter = new AtomicInteger(1)
  @volatile var blockarray = new AtomicReferenceArray[Block](par * 64)
  
  // cleaner thread, which periodically cleans the descriptor table
  val cleaner = if (!useCleaner) null else {
    val c = new Cleaner(this)
    c.start()
    c
  }
  
  /* methods */
  
  @inline def descriptor(): Descriptor = {
    val tid = Thread.currentThread.getId
    var hashed = tid.toInt
    if (hashed < 0) hashed = -hashed
    val idx = hashed % descs.length
    val d = descs(idx)
    if ((d ne null) && d.tid == tid) d
    else findDescriptor(tid, idx)
  }
  
  @tailrec def findDescriptor(tid: Long, start: Int): Descriptor = {
    var idx = start
    var count = 0
    val until = descs.length
    while (count < until) {
      val d = descs(idx)
      if (d ne null) {
        if (d.tid == tid && d.isLive) return d
        else if (tryRemove(descs, idx)) { /* retry with same idx */ }
        else {
          idx = (idx + 1) % descs.length
          count += 1
        }
      } else {
        val d = new Descriptor(tid, Thread.currentThread, idx)
        halfFill(d.active)
        descs(idx) = d
        descsize.getAndIncrement()
        
        // saving the descriptor in a threadlocal results in less garbage
        if (descindex.get != null) descindex.get.release()
        descindex.set(d.descriptorIndex)
        
        // see if load too high
        tryGrowDescriptorTable()
      }
    }
    tryGrowDescriptorTable()
    findDescriptor(tid, start)
  }
  
  def tryGrowDescriptorTable(): Unit = if ((1000 * descsize.get / descs.length) > 32) {
    // TODO
  }
  
  def tryRemove(descarray: Array[Descriptor], idx: Int): Boolean = {
    val d = descarray(idx)
    if (d != null && d.thread.getState == Thread.State.TERMINATED) {
      d.descriptorIndex.release()
      true
    } else false
  }
  
  def allocate(): R = {
    val d = descriptor()
    var pos = d.activePos
    if (pos > 0) {
      pos -= 1
      d.activePos = pos
      val obj = d.activeArray(pos)
      if (init ne null) init(obj)
      obj
    } else {
      decBlock(d)
      allocate()
    }
  }
  
  def dispose(f: R) {
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
      d.activePos = d.active.pos
    } else {
      // no previous block => top == active
      var block = stealBlock(d.tid)
      if (block eq null) block = allocateFullBlock()
      setNext(block, d.top)
      d.top = block
      d.active = block
      d.activeArray = d.active.array
      d.activePos = d.active.pos
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
      d.activePos = d.active.pos
    } else {
      releaseBlock(d.tid, d.top)
      d.top = d.active
      d.active = block
      d.activeArray = d.active.array
      d.activePos = d.active.pos
    }
  }
  
  def getNext(b: Block): Block = {
    b.next
  }
  
  def setNext(b: Block, next: Block) {
    b.next = next
  }
  
  def stealBlock(tid: Long): Block = {
    // do one round on the freepool and try to steal
    var attempts = 2
    var i = math.abs(tid.toInt) % par
    while (attempts > 0) {
      val b = stealAt(i)
      if (b ne null) return b
      i = (i + 1) % par
      attempts -= 1
    }
    
    null
  }
  
  def stealAt(pos: Int): Block = {
    val blockptr = freepool.get(pos)
    if ((blockptr & INDEX_MASK) == 0) null
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
  
  def releaseBlock(tid: Long, b: Block) {
    // loop until you manage to return the block
    var i = math.abs(tid.toInt) % par
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
    var idx = startsearch
    val barr = blockarray
    
    // resize if full
    if (idx >= barr.length) {
      tryGrowBlockArray(barr)
      allocblock(b, idx)
    } else if (barr.compareAndSet(idx, null, b)) {
      // success - set index for the block
      b.idx = idx
      
      // update block counter to highest value
      var blkcnt: Int = -1
      do {
        blkcnt = blockcounter.get
      } while (blkcnt <= idx && !blockcounter.compareAndSet(blkcnt, idx + 1))
    } else {
      // find next free entry
      while (barr.get(idx) != null && idx < barr.length) idx += 1
      allocblock(b, idx)
    }
  }
  
  def tryGrowBlockArray(oldbarr: AtomicReferenceArray[Block]) {
    // TODO
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
    for (i <- 0 until (BLOCKSIZE / 2)) array(i) = ctor
    b.pos = BLOCKSIZE / 2
  }
  
  def fill(b: Block) {
    val array = b.array
    for (i <- 0 until BLOCKSIZE) array(i) = ctor
    b.pos = BLOCKSIZE
  }
  
  /* debugging */
  
  def printState() {
    def ara2Array(ara: AtomicReferenceArray[Block]) = {
      val a = new Array[Block](ara.length)
      for (i <- 0 until ara.length) a(i) = ara.get(i)
      a
    }
    def ala2Array(ara: AtomicLongArray) = {
      val a = new Array[Long](ara.length)
      for (i <- 0 until ara.length) a(i) = ara.get(i)
      a
    }
    println("CPool")
    println("-----")
    println("Block array: " + (ara2Array(blockarray).zipWithIndex map {
      case (b, i) => if (b eq null) i + ": ___" else i + ": [" + b.idx + "]"
    } mkString(", ")))
    println("Block counter: " + blockcounter.get)
    val freepooltxt = ala2Array(freepool) map {
      ptr =>
      val stamp = (ptr & TIMESTAMP_MASK) >>> TIMESTAMP_OFFSET
      val idx = ptr & INDEX_MASK
      "(%d, %d)".format(stamp, idx)
    } mkString(", ")
    println("Freepool: " + freepooltxt)
    println("Descriptors: ")
    for (d <- descs; if d != null) {
      println("Tid: " + d.tid)
      println("Top: " + d.top.idx)
      println("Active: " + d.active.idx)
      println("Active pos: " + d.activePos)
      println()
    }
  }
  
}


final class Cleaner[R >: Null <: AnyRef: Manifest](_cpool: CPool[R]) extends Thread {
  val cpool = new java.lang.ref.WeakReference(_cpool)
  
  setDaemon(true)
  
  def MINCLEANPERIOD = 50
  def MAXCLEANPERIOD = 1600
  
  var cleanperiod = MINCLEANPERIOD
  var lastdeschash = 0
  
  override def run() {
    cleaningLoop()
  }
  
  @tailrec
  def cleaningLoop(): Unit = cpool.get match {
    case null => // we're done
    case cpool: CPool[R] =>
      val descs = cpool.descs
      var observeddeschash = 0
      var i = 0
      val until = descs.length
      while (i < until) {
        // update hash
        val d = descs(i)
        observeddeschash = (observeddeschash + 1) * (if (d eq null) -1 else d.tid.toInt)
        
        // try to clean
        cpool.tryRemove(descs, i)
        i += 1
      }
      
      // backoff if necessary
      if (lastdeschash == observeddeschash) {
        cleanperiod = math.min(cleanperiod * 2, MAXCLEANPERIOD)
      } else {
        cleanperiod = MINCLEANPERIOD
        lastdeschash = observeddeschash
      }
      
      // sleep
      //println("cleaned... sleep for: " + cleanperiod)
      Thread.sleep(cleanperiod)
      cleaningLoop()
  }
  
}





