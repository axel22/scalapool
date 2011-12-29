/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import collection._
import util._



abstract class SingleThreadPool extends MemoryPool {
  val poolmap = mutable.HashMap[Class[_], (() => Reclaimable, Reclaimable => Unit, Stack[Reclaimable])]()
  
  def newObjectStack: Stack[Reclaimable]
  
  /** Registers a new object type with the pool. */
  def register[T <: Reclaimable: ClassManifest](ctor: =>T)(init: T => Unit) = {
    val cls = implicitly[ClassManifest[T]].erasure
    poolmap.get(cls) match {
      case Some(_) => illegalarg("Object type %s already registered with the pool.".format(manifest))
      case None => poolmap put (cls, (() => ctor, init.asInstanceOf[Reclaimable => Unit], newObjectStack))
    }
  }
  
  /** Allocates an object of the requested type. */
  def allocate[T <: Reclaimable: ClassManifest](): T = {
    val manifest = implicitly[ClassManifest[T]]
    poolmap.get(manifest.erasure) map {
      case (ctor, init, pool) =>
        val obj = if (pool.nonEmpty) pool.pop() else ctor()
      init(obj)
      obj.memoryPool = this
      obj.asInstanceOf[T]
    } getOrElse illegalarg("Object type %s not registered with the pool.".format(manifest))
  }
  
  /** Called internally by the dispose - reclaims the object. */
  private[mempool] def reclaim(r: Reclaimable) {
    poolmap.get(r.getClass) match {
      case Some((_, _, pool)) => if (!pool.isFull) pool.push(r) else { /* take me to the place where GC shines */ }
      case None => illegalarg("Cannot reclaim this object type: %s.".format(r.getClass))
    }
  }
  
}


class SingleThreadUnlimitedPool extends SingleThreadPool {
  def newObjectStack = new UnrolledStack[Reclaimable]
}


class SingleThreadFixedPool(capacity: Int) extends SingleThreadPool {
  def newObjectStack = new FixedStack[Reclaimable](capacity)
}

