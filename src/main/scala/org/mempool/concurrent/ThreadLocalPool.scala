/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._



/** A memory pool which creates a unique memory pool for each thread.
 *  
 *  This memory pool allows allocating an object on one thread, and disposing it by a different thread.
 *  Publication of allocated objects is still the clients responsibility.
 */
class ThreadLocalPool[R](memoryPoolFactory: () => MemoryPool[R]) extends ConcurrentMemoryPool[R] {
  val localPool = new ThreadLocal[MemoryPool[R]] {
    override def initialValue = memoryPoolFactory()
  }
  
  def allocate(): R = {
    val obj = localPool.get.allocate()
    specialInitialize(obj)
    obj
  }
  
  def dispose(obj: R) = localPool.get.dispose(obj)
  
}






