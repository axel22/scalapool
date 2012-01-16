/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package concurrent



import annotation.unchecked._
import annotation.{tailrec, switch}



/** Describes objects which can be owned to concurrent memory pools.
 */
trait Poolable[R <: Poolable[R]] extends Disposable[R] {
  @volatile
  private[scalapool] var _memory_pool: ConcurrentMemoryPool[R] = _
  
  def dispose() {
    if (_memory_pool ne null) _memory_pool.dispose(this.asInstanceOf[R])
  }
  
}

