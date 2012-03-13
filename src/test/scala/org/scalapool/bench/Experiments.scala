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
  
  final def BLOCKSIZE = 256
  
  class Descriptor(val tid: Long) {
    var pos: Int = 128
    var array = new Array[Foo](BLOCKSIZE + 1)
    for (i <- 0 until 128) array(i) = new Foo
  }
  
  // deliberately not an AtomicRefArray, 'cause we can live with a weak get
  val descs = new Array[Descriptor](par * 64)
  
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
        descs(idx) = d
        // if (descs.compareAndSet(idx, null, d)) return d
      }
    }
    sys.error("unreachable code")
  }
  
  def allocate(): Foo = {
    val d = descriptor()
    d.pos -= 1
    if (d.pos >= 0) d.array(d.pos)
    else {
      sys.error("not implemented")
    }
  }
  
  def dispose(f: Foo) {
    val d = descriptor()
    if (d.pos < BLOCKSIZE) {
      d.array(d.pos) = f
      d.pos += 1
    } else {
      sys.error("not implemented")
    }
  }
  
}






