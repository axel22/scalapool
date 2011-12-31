/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package singlethread



import collection._
import util._



/** Base class for memory pools that use a non-thread safe stack for object storage.
 *  
 *  Stack-based pools allocate extra arrays for the stack, but have good cache locality.
 *  Pooled objects do not have to have any special housekeeping fields.
 */
abstract class StackPool[R: ClassManifest](ctor: =>R)(init: R => Unit)
extends MemoryPool[R] {
  def newObjectStack: Stack[R]
  
  val pool = newObjectStack
  
  /** Allocates an object of the requested type. */
  def allocate(): R = {
    val obj = if (pool.nonEmpty) pool.pop() else ctor
    init(obj)
    if (obj.isInstanceOf[Ownable[_]])
      obj.asInstanceOf[Ownable[Nothing]].memoryPool = this.asInstanceOf[MemoryPool[Nothing]]
    obj.asInstanceOf[R]
    // could try-catch and illegalarg("Object type %s not registered with the pool.".format(manifest))
  }
  
  /** Disposes the object and makes it eligible for allocation again. */
  def dispose(r: R) {
    if (!pool.isFull) pool.push(r) else { /* take me to the place where GC shines */ }
    // could try-catch and illegalarg("Cannot reclaim this object type: %s.".format(r.getClass))
  }
  
}


class UnlimitedPool[R: ClassManifest](c: =>R)(i: R => Unit) extends StackPool[R](c)(i) {
  def newObjectStack = new UnrolledStack[R]
}


class FixedPool[R: ClassManifest](capacity: Int)(c: =>R)(i: R => Unit) extends StackPool[R](c)(i) {
  def newObjectStack = new FixedStack[R](capacity)
}

