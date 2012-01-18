/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package concurrent



import annotation.unchecked._



/** A memory pool which creates a unique memory pool for each thread.
 *  
 *  This memory pool allows allocating an object on one thread, and disposing it by a different thread.
 *  Publication of allocated objects is still the clients responsibility.
 *  
 *  Useful when each thread allocates and disposes objects relatively independently - each thread
 *  has its own local memory pool and objects are not shared between these pools.
 *  In the situation where a producer thread constantly allocates objects and a consumer thread disposes them,
 *  an out of memory error will happen eventually, since the consumer pool will be filled and those objects
 *  never used up.
 *  
 *  Several times slower than heap-based allocation due to a thread local lookup, but much more scalable as more
 *  processors are being used.
 */
class ThreadLocalPool[R >: Null <: AnyRef](memoryPoolFactory: () => MemoryPool[R]) extends ConcurrentMemoryPool[R] {
  val localPool = new ThreadLocal[MemoryPool[R]] {
    override def initialValue = memoryPoolFactory()
  }
  val special = resolveInit(memoryPoolFactory().allocate())
  
  def allocate(): R = {
    val obj = localPool.get.allocate()
    special(obj)
    obj
  }
  
  def dispose(obj: R) = localPool.get.dispose(obj)
  
}






