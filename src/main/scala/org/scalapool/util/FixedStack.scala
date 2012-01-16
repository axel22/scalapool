/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package util






/** Fixed capacity stack.
 */
class FixedStack[T: ClassManifest](val capacity: Int) extends Stack[T] {
  private var sz = 0
  private val array = new Array[T](capacity)
  
  def push(x: T) = {
    array(sz) = x
    sz += 1
  }
  
  def pop() = {
    val r = array(sz - 1)
    sz -= 1
    r
  }
  
  override def isEmpty = sz == 0
  
  def isFull = sz == capacity
  
  def foreach[U](f: T => U) = {
    var i = sz
    while (i > 0) {
      i -= 1
      f(array(i))
    }
  }
  
  override def toString = "FixedStack(%s)".format(
    array.take(sz).mkString(", ")
  )
  
}


object FixedStack {
  
}



