/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool






/** Allocates objects that are disposable.
 *  
 *  The representation type of the objects can be specified.
 */
trait Allocator[DisposableVariant <: Disposable] {
  
  /** Allocates disposable objects.
   */
  def allocate[T <: DisposableVariant: ClassManifest](): T
  
}


object Allocator {
  
  def heap[Repr <: Disposable: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = new Allocator[Repr] {
    def allocate[T <: Repr: ClassManifest](): T =
      if (implicitly[ClassManifest[T]] == implicitly[ClassManifest[Repr]]) {
        val r = ctor.asInstanceOf[T]
        init(r)
        r
      } else illegalarg("This allocator can only allocate objects of type: " + implicitly[ClassManifest[Repr]])
  }
  
  def singleThreadUnlimitedPool[Repr <: Reclaimable: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
    val mempool = MemoryPool.singleThread
    mempool.register(ctor)(init)
    mempool
  }
  
  def singleThreadFixedPool[Repr <: Reclaimable: ClassManifest](capacity: Int)(ctor: =>Repr)(init: Repr => Unit) = {
    val mempool = MemoryPool.singleThreadFixed(capacity)
    mempool.register(ctor)(init)
    mempool
  }
  
}


