/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._



/** A concurrent memory pool.
 */
class CPool[R] extends ConcurrentMemoryPool[R] {
  
  def allocate(): R = {
    null.asInstanceOf[R]
  }
  
  def dispose(obj: R) {
    
  }
  
}






