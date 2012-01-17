/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import annotation.unchecked._
import concurrent.{Readable, Poolable}



/** Used to create reclaimable objects of a certain type concurrently.
 *  
 *  Pools of this type can allocate and dispose objects in a thread-safe manner.
 */
trait ConcurrentMemoryPool[R >: Null <: AnyRef] extends Allocator[R] {
  protected def specialInitialize(obj: R) {
    import concurrent.Acquirable
    if (obj.isInstanceOf[Poolable[_]]) {
      obj.asInstanceOf[Poolable[Null]]._memory_pool = this.asInstanceOf[ConcurrentMemoryPool[Null]]
    }
    if (obj.isInstanceOf[Acquirable[_]]) {
      // order is important!!
      obj.asInstanceOf[Acquirable[Null]].reallocateInit()
    }
    if (obj.isInstanceOf[Readable[_]]) {
      val readable = obj.asInstanceOf[Readable[_]]
      // this check because some nodes are allocated on the heap
      if (readable.stamp % 2 != 0) readable.incrementStamp()
    }
  }
}


object ConcurrentMemoryPool




