/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import annotation.unchecked._
import concurrent.{Acquirable, Readable, Poolable}



/** Used to create reclaimable objects of a certain type concurrently.
 *  
 *  Pools of this type can allocate and dispose objects in a thread-safe manner.
 */
trait ConcurrentMemoryPool[R >: Null <: AnyRef] extends Allocator[R] {
self =>
  
  def special: SpecialInitializer[R]
  
  private[scalapool] def resolveInit(obj: R): SpecialInitializer[R] = {
    val si = obj match {
      case _: Acquirable[_] => new SpecialInitializer[Acquirable[Null]] {
        def apply(obj: Acquirable[Null]) {
          obj._memory_pool = self.asInstanceOf[ConcurrentMemoryPool[Null]]
          // order is important!!
          obj.reallocateInit()
        }
      }
      case _: Readable[_] => new SpecialInitializer[Readable[Null]] {
        def apply(obj: Readable[Null]) {
          obj._memory_pool = self.asInstanceOf[ConcurrentMemoryPool[Null]]
          // this check because some nodes are allocated on the heap
          if (obj.stamp % 2 != 0) obj.incrementStamp()
        }
      }
      case _: Poolable[_] => new SpecialInitializer[Poolable[Null]] {
        def apply(obj: Poolable[Null]) {
          obj._memory_pool = self.asInstanceOf[ConcurrentMemoryPool[Null]]
        }
      }
      case _ => new SpecialInitializer[R] {
        def apply(obj: R) {}
      }
    }
    
    si.asInstanceOf[SpecialInitializer[R]]
  }
}


object ConcurrentMemoryPool




