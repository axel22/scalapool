/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package singlethread



import collection._
import util._



/** Base class for memory pools that use a non-thread safe stack for object storage.
 *  
 *  Stack-based pools allocate extra arrays for the stack, but have good cache locality.
 *  Pooled objects do not have to have any special housekeeping fields.
 */
abstract class StackPool[R >: Null <: AnyRef: ClassManifest](ctor: =>R)(init: R => Unit)
extends MemoryPool[R] {
  def newObjectStack: Stack[R]
  
  val pool = newObjectStack
  
  /** Allocates an object of the requested type. */
  def allocate(): R = {
    var obj = pool.pop()
    if (obj eq null) obj = ctor
    init(obj)
    special(obj)
    obj.asInstanceOf[R]
    // could try-catch and illegalarg("Object type %s not registered with the pool.".format(manifest))
  }
  
  /** Disposes the object and makes it eligible for allocation again. */
  def dispose(r: R) {
    if (!pool.isFull) pool.push(r) else { /* take me to the place where GC shines */ }
    // could try-catch and illegalarg("Cannot reclaim this object type: %s.".format(r.getClass))
  }
  
}


/** A single thread memory pool generating a neglectible amount of garbage.
 */
class UnlimitedPool[R >: Null <: AnyRef: ClassManifest](c: =>R)(i: R => Unit) extends StackPool[R](c)(i) {
  val special = resolveInit(c)
  def newObjectStack = new UnrolledStack[R]
}


/** A single thread memory pool generating garbage only if its capacity is exceeded.
 */
class FixedPool[R >: Null <: AnyRef: ClassManifest](capacity: Int)(c: =>R)(i: R => Unit) extends StackPool[R](c)(i) {
  val special = resolveInit(c)
  def newObjectStack = new FixedStack[R](capacity)
}


/** A single thread memory pool which can grow - it generates garbage as it grows. It is never shrinked,
 *  so eventually it stops generating garbage, but its memory usage is never reduced.
 */
class GrowingPool[R >: Null <: AnyRef: ClassManifest](c: =>R)(i: R => Unit) extends StackPool[R](c)(i) {
  val special = resolveInit(c)
  def newObjectStack = new GrowingStack[R]
}
