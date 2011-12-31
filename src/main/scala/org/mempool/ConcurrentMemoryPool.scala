/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import annotation.unchecked._



/** Used to create reclaimable objects of a certain type, and from different threads.
 *  
 *  Pools of this type can allocate and dispose objects in a thread-safe manner.
 */
trait ConcurrentMemoryPool[R] extends Allocator[R] {
  /** Allocates an object */
  def allocate(): R
  
  /** Reclaims the object. Called internally by `dispose`. */
  def dispose(r: R @uncheckedVariance): Unit
}


object ConcurrentMemoryPool {
  
}




