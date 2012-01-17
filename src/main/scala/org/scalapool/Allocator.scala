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
abstract class Allocator[R] {
  
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
  
  def heap[Repr: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = new Allocator[Repr] {
    def allocate(): Repr =
      if (implicitly[ClassManifest[Repr]] == implicitly[ClassManifest[Repr]]) {
        val r = ctor.asInstanceOf[Repr]
        init(r)
        r
      } else illegalarg("This allocator can only allocate objects of type: " + implicitly[ClassManifest[Repr]])
    def dispose(obj: Repr) { /* do absolutely nothing */ }
  }
  
  object singleThread {
    
    def unlimitedPool[Repr: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.UnlimitedPool[Repr](ctor)(init)(resolveInit(implicitly[ClassManifest[_]].erasure))
      mempool
    }
    
    def fixedPool[Repr: ClassManifest](capacity: Int)(ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.FixedPool[Repr](capacity)(ctor)(init)(resolveInit(implicitly[ClassManifest[_]].erasure))
      mempool
    }
    
    def growingPool[Repr: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.GrowingPool[Repr](ctor)(init)(resolveInit(implicitly[ClassManifest[_]].erasure))
      mempool
    }
    
    def freeList[Repr <: singlethread.Linkable[Repr]: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
      val mempool = new singlethread.FreeList[Repr](ctor)(init)(resolveInit(implicitly[ClassManifest[_]].erasure))
      mempool
    }
  }
  
  object concurrent {
    import org.scalapool.concurrent._
    
    def threadLocalPool[Repr: ClassManifest](memoryPoolFactory: =>MemoryPool[Repr]) = {
      new ThreadLocalPool(() => memoryPoolFactory)
    }
  }
  
}


