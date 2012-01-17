/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import annotation.unchecked._
import org.scalapool.singlethread.{Poolable, Linkable}



/** Used to create reclaimable objects of a certain type.
 *  
 *  The memory pool allocates objects from its internal storage or by
 *  allocating additional memory, as needed.
 */
abstract class MemoryPool[R >: Null <: AnyRef] extends Allocator[R] {
self =>
  
  def special: SpecialInitializer[R]
  
  private[scalapool] def resolveInit(obj: R @uncheckedVariance): SpecialInitializer[R] = {
    val si = obj match {
      case p: Poolable[_] with Linkable[_] => new SpecialInitializer[Poolable[R] with Linkable[R]] {
        def apply(obj: Poolable[R] with Linkable[R]) {
          obj._memory_pool = self
          obj._linkable_next = null.asInstanceOf[R]
        }
      }
      case p: Linkable[_] => new SpecialInitializer[Linkable[R]] {
        def apply(obj: Linkable[R]) {
          obj._linkable_next = null.asInstanceOf[R]
        }
      }
      case p: Poolable[_] => new SpecialInitializer[Poolable[R]] {
        def apply(obj: Poolable[R]) {
          obj._memory_pool = self
        }
      }
      case _ => new SpecialInitializer[R] {
        def apply(obj: R) {}
      }
    }
    si.asInstanceOf[SpecialInitializer[R]]
  }
  
}


object MemoryPool




