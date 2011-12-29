/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool






/** Describes objects which can be reclaimed. */
trait Reclaimable extends Disposable {
  
  var memoryPool: MemoryPool = _
  
  /** Reclaims this object.
   *  
   *  A reclaimed object may not be used in the application again.
   *  Objects implementing this trait are stored in a
   *  memory pool after `dispose` has been called, so that they
   *  could be reinitialized again without going through a GC cycle.
   */
  def dispose() = if (memoryPool ne null) memoryPool.reclaim(this)
  
}


