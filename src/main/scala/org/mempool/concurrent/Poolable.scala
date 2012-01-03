/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._
import annotation.{tailrec, switch}



/** Describes objects which can be owned to concurrent memory pools.
 */
trait Poolable[R <: Poolable[R]] extends Disposable[R] {
  @volatile
  private[mempool] var memoryPool: ConcurrentMemoryPool[R] = _
  
  def dispose() {
    if (memoryPool ne null) memoryPool.dispose(this.asInstanceOf[R])
  }
  
}

