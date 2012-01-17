/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package concurrent



import annotation.unchecked._



/** A concurrent memory pool.
 */
class CPool[R >: Null <: AnyRef](ctor: =>R)(init: R => Unit) extends ConcurrentMemoryPool[R] {
  
  val special = resolveInit(ctor)
  
  def allocate(): R = {
    null.asInstanceOf[R]
  }
  
  def dispose(obj: R) {
    
  }
  
}






