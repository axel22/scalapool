/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import annotation.unchecked._



/** Used to create reclaimable objects of a certain type.
 *  
 *  The memory pool allocates objects from its internal storage or by
 *  allocating additional memory, as needed.
 */
trait MemoryPool[R] extends Allocator[R] {
  protected def specialInitialize(obj: R) {
    import singlethread.Poolable
    if (obj.isInstanceOf[Poolable[_]])
      obj.asInstanceOf[Poolable[Nothing]].memoryPool = this.asInstanceOf[MemoryPool[Nothing]]
  }
}


object MemoryPool




