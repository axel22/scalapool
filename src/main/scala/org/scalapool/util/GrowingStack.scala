/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package util






/** Growing capacity stack.
 */
class GrowingStack[T >: Null <: AnyRef: ClassManifest] extends Stack[T] {
  private var sz = 0
  private var array = new Array[T](16)
  
  private def ensureSize() {
    if (sz == array.length) {
      val narr = new Array[T](array.length * 2)
      Array.copy(array, 0, narr, 0, array.length)
      array = narr
    }
  }
  
  def push(x: T) = {
    ensureSize()
    array(sz) = x
    sz += 1
  }
  
  def pop() = if (sz == 0) null else {
    val r = array(sz - 1)
    sz -= 1
    r
  }
  
  override def isEmpty = sz == 0
  
  def isFull = false
  
  def foreach[U](f: T => U) = {
    var i = sz
    while (i > 0) {
      i -= 1
      f(array(i))
    }
  }
  
  override def toString = "GrowingStack(%s)".format(
    array.take(sz).mkString(", ")
  )
  
}


object GrowingStack {
  
}



