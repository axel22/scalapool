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
  
  def singleThreadUnlimitedPool[Repr: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
    val mempool = new singlethread.UnlimitedPool[Repr](ctor)(init)
    mempool
  }
  
  def singleThreadFixedPool[Repr: ClassManifest](capacity: Int)(ctor: =>Repr)(init: Repr => Unit) = {
    val mempool = new singlethread.FixedPool[Repr](capacity)(ctor)(init)
    mempool
  }
  
  def singleThreadFreeList[Repr <: singlethread.Linkable[Repr]: ClassManifest](ctor: =>Repr)(init: Repr => Unit) = {
    val mempool = new singlethread.FreeList[Repr](ctor)(init)
    mempool
  }
  
}


