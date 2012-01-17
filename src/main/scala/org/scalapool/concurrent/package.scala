/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import annotation.unchecked._



package object concurrent {
  @inline def acquire[R >: Null <: Acquirable[R]](obtain: =>R): R = {
    var loop = true
    var r: R = null.asInstanceOf[R]
    do {
      r = obtain
      if (r ne null) {
        if (r.check(r.acquire(), obtain)) loop = false
      } else loop = false
    } while (loop)
    
    r
  }
  
  @inline def release[R >: Null <: Acquirable[R]](r: R) = r.release()
  
  @inline def acquiring[R >: Null <: Acquirable[R], T](obtain: =>R)(body: R => T): T = {
    val r = acquire(obtain)
    val t = body(r)
    release(r)
    t
  }
  
  @inline def reading[R >: Null <: Readable[R], T](obtain: =>R)(body: R => T): T = {
    var loop = true
    var t: T = null.asInstanceOf[T]
    do {
      val r = obtain
      if (r ne null) {
        val stamp = r.stamp
        if (stamp % 2 == 0) {
          t = body(r)
          if (r.stamp == stamp) loop = false
        }
      } else {
        t = body(r)
        loop = false
      }
    } while (loop)
    
    t
  }
  
}



