/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package singlethread






/** A memory pool which holds objects in a linked list, called free-list.
 *  
 *  Objects have to have a special field for the next object in the free-list,
 *  thus having to mix in the `Linkable[R]` trait, where `R` is their type.
 *  
 *  This memory pool does not allocate extra memory to keep track of the objects,
 *  but requires a special field in the objects and does not have memory-locality
 *  guarantees.
 */
class FreeList[R <: Linkable[R]](ctor: =>R)(init: R => Unit) extends MemoryPool[R] {
  
  private var freelist: R = _
  
  def allocate(): R = {
    val obj = if (freelist eq null) ctor else {
      val result = freelist
      freelist = freelist._linkable_next
      result
    }
    init(obj)
    specialInitialize(obj)
    obj
  }
  
  def dispose(obj: R) {
    obj._linkable_next = freelist
    freelist = obj
  }
  
}
