/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool






/** Allocates objects that are disposable.
 *  
 *  The representation type of the objects can be specified.
 */
abstract class Allocator[R >: Null <: AnyRef] {
  
  /** Allocates disposable objects.
   */
  def allocate(): R
  
  /** Disposes a previously allocated object.
   *  
   *  Unless specified differently by the allocater implementation,
   *  the object to be disposed has to be previously allocated by
   *  the same allocator.
   */
  def dispose(obj: R): Unit
  
}


object Allocator {
  
  def heap[Repr >: Null <: AnyRef: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = new Allocator[Repr] {
    def allocate(): Repr =
      if (implicitly[ClassManifest[Repr]] == implicitly[ClassManifest[Repr]]) {
        val r = ctor.asInstanceOf[Repr]
        init(r)
        r
      } else illegalarg("This allocator can only allocate objects of type: " + implicitly[ClassManifest[Repr]])
    def dispose(obj: Repr) { /* do absolutely nothing */ }
  }
  
  object singleThread {
    
    def unrolledPool[Repr >: Null <: AnyRef: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.UnrolledPool[Repr](ctor)(init)
      mempool
    }
    
    def fixedPool[Repr >: Null <: AnyRef: ClassManifest](ctor: =>Repr)(capacity: Int)(init: Repr => Unit) = {
      val mempool = new singlethread.FixedPool[Repr](capacity)(ctor)(init)
      mempool
    }
    
    def growingPool[Repr >: Null <: AnyRef: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.GrowingPool[Repr](ctor)(init)
      mempool
    }
    
    def freeList[Repr >: Null <: singlethread.Linkable[Repr]: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.FreeList[Repr](ctor)(init)
      mempool
    }
  }
  
  object concurrent {
    import org.scalapool.concurrent._
    
    def threadLocalPool[Repr >: Null <: AnyRef: ClassManifest](memoryPoolFactory: =>MemoryPool[Repr]) = {
      new ThreadLocalPool(() => memoryPoolFactory)
    }
    
    def cpool[Repr >: Null <: AnyRef: Manifest](par: Int, useCleaner: Boolean = true)(ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new CPool[Repr](par, useCleaner)(ctor)(init)
      mempool
    }
  }
  
}


