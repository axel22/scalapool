/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import annotation.unchecked._
import concurrent.{Readable, Poolable}



/** Used to create reclaimable objects of a certain type concurrently.
 *  
 *  Pools of this type can allocate and dispose objects in a thread-safe manner.
 */
trait ConcurrentMemoryPool[R] extends Allocator[R] {
  protected def specialInitialize(obj: R) {
    import concurrent.Acquirable
    if (obj.isInstanceOf[Poolable[_]]) {
      obj.asInstanceOf[Poolable[Nothing]].memoryPool = this.asInstanceOf[ConcurrentMemoryPool[Nothing]]
    }
    if (obj.isInstanceOf[Acquirable[_]]) {
      // order is important!!
      obj.asInstanceOf[Acquirable[Nothing]].reallocateInit()
    }
    if (obj.isInstanceOf[Readable[_]]) {
      obj.asInstanceOf[Readable[_]].incrementStamp()
    }
  }
}


object ConcurrentMemoryPool




