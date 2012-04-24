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
    var active: Block = null
    var top: Block = null
    var activeArray: Array[R] = null
    var activePos: Int = 0
    
    def isLive = descriptorIndex.lastd ne null
    def initActive(b: Block) {
      active = b
      top = active
      activeArray = active.array
      activePos = active.pos
    }
    
    initActive(fetchFullBlock(tid))
    
    def printOut() {
      println("Tid: " + tid)
      println("Top: " + top.idx)
      println("Active: " + active.idx)
      println("Active pos: " + activePos)
      println("top length: " + lengthBlocks(top))
      printBlocks(top)
      println()
    }
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
    
    def releaseDescriptor() {
      val d = lastd
      
      if (d eq null) { /* we're done */ }
      else if (CAS_lastd(d, null)) { // ensure only 1 release!
        // return blocks in `d` to freepool
        if (d.active ne d.top) releaseBlock(d.top.##, d.top)
        d.active.pos = d.activePos
        releaseBlock(d.active.##, d.active)
        
        // remove descriptor (very small chance to create garbage)
        descriptors(idx) = null
        descsize.getAndDecrement()
      } else releaseDescriptor()
    }
    
    override def finalize() = releaseDescriptor()

  }
  
  final class BlockPool {
    // pool of free object blocks
    val pool = new AtomicLongArray(par)
    
    def steal(hash: Long): Block = {
      // do 4 rounds on the freepool and try to steal
      var attempts = par * 4
      var i = math.abs(hash.toInt) % par
      while (attempts > 0) {
        val b = stealAt(i)
        if (b ne null) return b
        i = (i + 1) % par
        attempts -= 1
      }
      
      null
    }
    
    private def stealAt(pos: Int): Block = {
      val blockptr = pool.get(pos)
      if ((blockptr & INDEX_MASK) == 0) null
      else {
        val stamp = blockptr & TIMESTAMP_MASK
        val nstamp = stamp + TIMESTAMP_GROWTH
        val idx = blockptr & INDEX_MASK
        val block = blockarray.get(idx.toInt)
        val nblock = block.next
        val nidx = if (nblock eq null) 0L else nblock.idx.toLong & ((1L << 32) - 1)
        val nblockptr = nstamp | nidx
        if (pool.compareAndSet(pos, blockptr, nblockptr)) {
          block.next = null
          block
        } else null
      }
    }
    
    def release(hash: Long, b: Block) {
      // loop until you manage to return the block
      var i = math.abs(hash.toInt) % par
      while (true) {
        if (releaseAt(i, b)) return
        i = (i + 1) % par
      }
    }
    
    private def releaseAt(pos: Int, block: Block): Boolean = {
      val oblockptr = pool.get(pos)
      val ostamp = oblockptr & TIMESTAMP_MASK
      val nstamp = ostamp + TIMESTAMP_GROWTH
      val oidx = oblockptr & INDEX_MASK
      val oblock = if (oidx == 0) null else blockarray.get(oidx.toInt)
      val nidx = block.idx.toLong & ((1L << 32) - 1)
      val nblockptr = nstamp | nidx
      block.next = oblock
      pool.compareAndSet(pos, oblockptr, nblockptr)
    }
    
    override def toString = {
      val pooltxt = ala2Array(pool) map {
        ptr =>
        val stamp = (ptr & TIMESTAMP_MASK) >>> TIMESTAMP_OFFSET
        val idx = ptr & INDEX_MASK
        "(%d, %s)".format(stamp, idx + " -> elems: " + lengthBlocks(blockarray.get(idx.toInt)))
      } mkString(", ")
      pooltxt
    }
    
  }
  
  /* fields */
  
  val special = resolveInit(ctor)
  
  // descs
  // is deliberately not an AtomicRefArray
  // 'cause we can live with a weak get - which is faster
  @volatile var descriptors = new Array[Descriptor](par * 32)
  val descsize = new AtomicInteger(0)
  val descindex = new ThreadLocal[DescriptorIndex] {
    override def initialValue = null
  }
  val descriptorsUpdater = AtomicReferenceFieldUpdater.newUpdater(classOf[CPool[R]], classOf[Array[Descriptor]], "descriptors")
  
  val freepool = new BlockPool
  val emptypool = new BlockPool
  val halfpool = new BlockPool
  
  // blocks array
  val blockcounter = new AtomicInteger(1)
  @volatile var blockarray = new AtomicReferenceArray[Block](par * 64)
  val blockarrayUpdater = AtomicReferenceFieldUpdater.newUpdater(classOf[CPool[R]], classOf[AtomicReferenceArray[Block]], "blockarray")
  
  // cleaner thread, which periodically cleans the descriptor table
  val cleaner = if (!useCleaner) null else {
    val c = new Cleaner(this)
    c.start()
    c
  }
  
  /* methods */
  
  @inline def descriptor(): Descriptor = {
    val descs = descriptors
    val tid = Thread.currentThread.getId
    var hashed = tid.toInt
    if (hashed < 0) hashed = -hashed
    val idx = hashed % descs.length
    val d = descs(idx)
    if ((d ne null) && d.tid == tid) d
    else findDescriptor(tid)
  }
  
  @tailrec def findDescriptor(tid: Long): Descriptor = {
    val descs = descriptors
    var hashed = tid.toInt
    if (hashed < 0) hashed = -hashed
    var idx = hashed % descs.length
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
        // saving the descriptor in a threadlocal results in less garbage
        if (descindex.get != null) descindex.get.releaseDescriptor()
        
        val d = new Descriptor(tid, Thread.currentThread, idx)
        descs(idx) = d
        descsize.getAndIncrement()
        descindex.set(d.descriptorIndex)
        
        // see if load too high
        tryGrowDescriptorTable()
        
        return d
      }
    }
    tryGrowDescriptorTable()
    findDescriptor(tid)
  }
  
  def tryGrowDescriptorTable(): Unit = {
    val descs = descriptors
    val dsize = descsize.get
    if ((1000 * dsize / descs.length) > 32) {
      // println("growing descriptor table: ")
      // println("observed size: " + dsize)
      // println("descs length: " + descs.length)
      val ndescs = new Array[Descriptor](descs.length * 2)
      if (descriptorsUpdater.compareAndSet(this, descs, ndescs)) {
        // println("managed to set to: " + ndescs.length)
      }
    }
  }
  
  def tryRemove(descarray: Array[Descriptor], idx: Int): Boolean = {
    val d = descarray(idx)
    if (d != null && d.thread.getState == Thread.State.TERMINATED) {
      d.descriptorIndex.releaseDescriptor()
      true
    } else false
  }
  
  def allocate(): R = {
    val d = descriptor()
    var pos = d.activePos
    if (pos > 0) {
      pos -= 1
      d.activePos = pos
      val array = d.activeArray
      val obj = array(pos)
      //array(pos) = null
      if (init ne null) init(obj)
      obj
    } else {
      d.active.pos = 0
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
      d.active.pos = BLOCKSIZE
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
      // release any block two levels below top
      //printDescriptors()
      val twobelow = getNext(d.active)
      if (twobelow ne null) {
        d.active.next = null
        //assert(twobelow.array.forall(_ == null), (twobelow.pos, twobelow.array.mkString(", ")))
        emptypool.release(twobelow.##, twobelow)
      }
      
      // no previous block => top == active
      val block = fetchFullBlock(d.active.##)
      setNext(block, d.top)
      d.top = block
      d.active = block
      d.activeArray = d.active.array
      d.activePos = d.active.pos
    }
    //println("dec block: " + d.tid + ", actpos: " + d.activePos)
  }
  
  def incBlock(d: Descriptor) {
    var block = getNext(d.active)
    if (block eq null) {
      block = emptypool.steal(d.active.##)
      if (block eq null) block = allocateEmptyBlock()
      setNext(d.active, block)
    }
    if (d.active eq d.top) {
      d.active = block
      d.activeArray = d.active.array
      d.activePos = d.active.pos
    } else {
      freepool.release(d.top.##, d.top)
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
  
  def releaseBlock(hash: Long, b: Block) {
    if (b.pos == 0) {
      //assert(b.array.forall(_ == null), (b.pos, b.array.mkString(", ")))
      emptypool.release(hash, b)
    } else if (b.pos == BLOCKSIZE) freepool.release(hash, b)
    else halfpool.release(hash, b)
  }
  
  def fetchFullBlock(hash: Long) = {
    var b = freepool.steal(hash)
    if (b eq null) {
      b = emptypool.steal(hash)
      if (b eq null) b = allocateFullBlock() else fill(b)
    }
    b
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
      while (idx < barr.length && barr.get(idx) != null) idx += 1
      allocblock(b, idx)
    }
  }
  
  def tryGrowBlockArray(oldbarr: AtomicReferenceArray[Block]) {
    val nbarr = new AtomicReferenceArray[Block](oldbarr.length * 2)
    var i = 0
    while (i < oldbarr.length) {
      nbarr.set(i, oldbarr.get(i))
      i += 1
    }
    while (blockarray eq oldbarr) blockarrayUpdater.compareAndSet(this, oldbarr, nbarr)
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
  
  def fill(b: Block) {
    val array = b.array
    //assert(array.forall(_ == null), (b.pos, array.mkString(", ")))
    for (i <- 0 until BLOCKSIZE) array(i) = ctor
    counter.addAndGet(BLOCKSIZE)
    b.pos = BLOCKSIZE
  }
  
  /* debugging */
  
  val counter = new java.util.concurrent.atomic.AtomicInteger(0)
  
  def printState() {
    println("CPool")
    println("-----")
    println("Total allocations: " + counter.get())
    //printBlockArray()
    printBlockArraySize()
    println("Freepool: " + freepool)
    println("Emptypool: " + emptypool)
    println("Halfpool: " + halfpool)
    printDescriptors()
  }
  
  private def lengthBlocks(block: Block) = {
    var b = block
    var len = 0
    while (b ne null) {
      len += 1
      b = b.next
    }
    len
  }
  
  private def printBlocks(block: Block) {
    var b = block
    while (b ne null) {
      print(b.idx)
      print("[%d]".format(b.pos))
      print(" -> ")
      b = b.next
    }
    println("null")
  }
  
  private def ara2Array(ara: AtomicReferenceArray[Block]) = {
    val a = new Array[Block](ara.length)
    for (i <- 0 until ara.length) a(i) = ara.get(i)
    a
  }
  
  private def ala2Array(ara: AtomicLongArray) = {
    val a = new Array[Long](ara.length)
    for (i <- 0 until ara.length) a(i) = ara.get(i)
    a
  }
  
  def printBlockArraySize() {
    println("Block array size: " + blockarray.length)
    println("Block counter: " + blockcounter.get)
  }
  
  def printBlockArray() {
    println("Block array: " + (ara2Array(blockarray).zipWithIndex map {
      case (b, i) => if (b eq null) i + ": ___" else i + ": [" + b.idx + "]"
    } mkString(", ")))
    println("Block counter: " + blockcounter.get)
  }
  
  def printDescriptors() {
    println("Descriptors: ")
    for (d <- descriptors; if d != null) {
      d.printOut()
    }
  }
  
}


final class Cleaner[R >: Null <: AnyRef: Manifest](_cpool: CPool[R]) extends Thread {
  val cpool = new java.lang.ref.WeakReference(_cpool)
  
  setDaemon(true)
  setName("CPool-cleaner-" + getId)
  
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
      val descs = cpool.descriptors
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





