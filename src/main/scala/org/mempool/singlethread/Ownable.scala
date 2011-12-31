/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package singlethread



import annotation.unchecked._



/** Describes objects which can become owned by memory pools on creation.
 *  This allows disposing the object without a reference to a memory pool in the client code.
 */
trait Ownable[+R <: Ownable[R]] extends Disposable[R] {
  
  private[mempool] var memoryPool: MemoryPool[R @uncheckedVariance] = _
  
  def repr = this.asInstanceOf[R]
  
  /** Reclaims this object by the memory pool owning it.
   *  
   *  A reclaimed object may not be used in the application again.
   *  Objects implementing this trait are stored in a
   *  memory pool after `dispose` has been called, so that they
   *  could be reinitialized again without going through a GC cycle.
   */
  def dispose() = if (memoryPool ne null) memoryPool.dispose(repr)
  
}


