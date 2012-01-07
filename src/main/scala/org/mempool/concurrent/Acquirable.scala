/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._
import annotation.{tailrec, switch}



/** Describes objects which can be acquired concurrently, and have their
 *  `dispose` effects postponed until the reference to the object is lost.
 *  Acquired objects can be safely side-effected - their fields can be written
 *  and side-effects can be done based on the values of the fields.
 *  
 *  While this assumes a certain amount of discipline on the programmer's part,
 *  it ensures that an object referenced by a thread is not recycled by some
 *  other thread.
 *  
 *  Here is a use-case example.
 *  
 *  Assume there is a method `obtainReference()` which returns a reference
 *  to a concurrently acuirable object. This method is not side-effecting,
 *  and returns the same reference if called twice, provided that a 3rd thread
 *  has not done any writes.
 *  Before using the object, it has to be acquired. After obtaining the reference,
 *  and before acquiring it there is a possibility that some other thread disposes
 *  the reference (and then even reallocates it).
 *  For this reason, the reference has to be obtained again and then checked:
 *  
 *  {{{
 *  def doSomethingWithReference() {
 *    val r = obtainReference()
 *    if (r.check(r.acquire(), obtainReference())) {
 *      doWork(r)
 *      r.release() // release at the end
 *    } else doSomethingWithReference() // restart
 *  }
 *  }}}
 *  
 *  While this works, it is a programmatic overkill, and at least error-prone.
 *  The above procedure should only be used by experts.
 *  To avoid this boilerplate, the following constructs can be used:
 *  
 *  {{{
 *  val r = acquire { obtainReference() }
 *  doWork(r)
 *  release(r)
 *  }}}
 *  
 *  A block-style acquire is also possible using the `acquiring` block:
 *  
 *  {{{
 *  acquiring(obtainReference()) {
 *    r => doWork(r)
 *  }
 *  }}}
 *  
 *  These constructs are marked with the `@inline` annotation,
 *  so no closure is allocated for these objects provided that
 *  the `-Yinline` compiler option is used.
 *  Scala 2.9.1. actually inlines these methods but generates
 *  a `new` call to construct closure object nonetheless (which
 *  is then not used), so turning on escape analysis in the JVM
 *  may be essential.
 *  
 *  What the `acquire`/`release` pair ensures is that even if another thread
 *  calls `dispose` in the meanwhile, the object will not be disposed until
 *  all the threads that called `acquire` call `release` on the object.
 */
abstract class Acquirable[R <: Acquirable[R]] extends JAcquirable with Poolable[R] {
  import Acquirable._
  import JAcquirable._
  
  /* States:
   * 00 - disposed
   * 01 - allocated
   * 10 - pending-dispose
   * 
   * Transitions:
   * 00 -> 01 -> 10
   *  ^     |     |
   *  |     |     |
   *  \-----/<----/
   */
  
  def repr = this.asInstanceOf[R]
  
  reallocateInit()
  
  /** Called by the pool on reallocation.
   */
  private[mempool] def reallocateInit() {
    @tailrec def tryAllocate() {
      val rc = /*READ*/refcount
      if (rc == 0x00000000) {
        if (updater.compareAndSet(this, rc, 0x40000000)) {}
        else tryAllocate()
      } else ??? // if reallocating, the state has to be 00
    }
    
    if (/*READ*/refcount == 0x40000000) {} // do nothing - normal ctor
    else tryAllocate()
  }
  
  /** Acquires the object, returning a boolean indicating if the
   *  object is successfully acquired.
   */
  def acquire(): Boolean = {
    @tailrec def tryAcquire(): Boolean = {
      val rc = /*READ*/refcount
      val state = rc & statemask
      (state: @switch) match {
        case 0x00000000 => false // can't acquire disposed object
        case 0x40000000 | 0x80000000 =>
          val count = rc & countmask
          val ncount = count + 1
          val nrc = state | ncount
          if (updater.compareAndSet(this, rc, nrc)) true
          else tryAcquire()
      }
    }
    
    tryAcquire()
  }
  
  /** Given the result of the `acquire` call
   *  and the reference obtained once again after calling the
   *  `acquire` method, this method will check if the object
   *  has been correctly acquired, and if it has not been disposed
   *  since the last `acquire` call.
   *  If the object was acquired successfully, but the reference
   *  where it was obtained from changed, the object is released
   *  (there is no need to call `release`).
   *  
   *  If this method returns `true`, the object can be used safely,
   *  but has to be released manually by calling the `release` method.
   */
  def check(stamp: Boolean, r: R): Boolean = {
    if (!stamp) false
    else if (this ne r) {
      this.release()
      false
    } else true
  }
  
  /** Releases the previously successfully acquired object.
   */
  def release() {
    @tailrec def tryPrepare(): Boolean = {
      val rc = /*READ*/refcount
      val state = rc & statemask
      (state: @switch) match {
        case 0x00000000 => ??? // can't release disposed
        case 0x40000000 | 0x80000000 =>
          val count = rc & countmask
          if (count == 0) ??? // can't release unacquired object
          val ncount = count - 1
          val nrc = state | ncount
          if (updater.compareAndSet(this, rc, nrc)) state == 0x80000000
          else tryPrepare()
      }
    }
    
    if (tryPrepare()) disposeToMemoryPool()
  }
  
  private def disposeToMemoryPool() {
    if (memoryPool ne null) memoryPool.dispose(repr)
  }
  
  final override def dispose() {
    @tailrec def tryPrepare(): Boolean = {
      val rc = /*READ*/refcount
      (rc & statemask: @switch) match {
        case 0x00000000 => ??? // can't dispose twice
        case 0x80000000 => ??? // can't schedule for dispose twice
        case 0x40000000 =>
          val count = rc & countmask
          val nrc = if (count == 0) 0x00000000 else 0x80000000 | count
          if (updater.compareAndSet(this, rc, nrc)) nrc == 0x00000000
          else tryPrepare()
      }
    }
    
    if (tryPrepare()) disposeToMemoryPool()
  }
  
}


object Acquirable {
  private[mempool] val statemask = 0xc0000000
  private[mempool] val countmask = 0x3fffffff
}



