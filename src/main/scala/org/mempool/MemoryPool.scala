/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import collection._



/** Used to create reclaimable objects of a certain type.
 *  
 *  The memory pool allocates objects from its internal storage or by
 *  allocating additional memory, as needed.
 */
trait MemoryPool extends Allocator[Reclaimable] {
  /** Registers a new object type with the pool. */
  def register[T <: Reclaimable: ClassManifest](ctor: =>T)(init: T => Unit)
  
  /** Allocates an object */
  def allocate[T <: Reclaimable: ClassManifest](): T
  
  /** Reclaims the object. Called internally by `dispose`. */
  private[mempool] def reclaim(r: Reclaimable): Unit
}


object MemoryPool {
  
  def singleThread: MemoryPool = new SingleThreadUnlimitedPool
  
  def singleThreadFixed(capacity: Int): MemoryPool = new SingleThreadFixedPool(capacity)
  
}




