/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool.util





/** Abstract stack.
 */
trait Stack[T] extends Traversable[T] {
  def push(x: T): Unit
  
  def pop(): T
  
  def isEmpty: Boolean
  
  def isFull: Boolean
  
  def foreach[U](f: T => U): Unit
  
}




