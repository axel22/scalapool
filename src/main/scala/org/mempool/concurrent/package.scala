/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool



import annotation.unchecked._



package object concurrent {
  @inline def acquire[R <: Referable[R]](obtain: =>R): R = {
    var loop = true
    var r: R = null.asInstanceOf[R]
    do { 
      r = obtain
      if (r.check(r.acquire(), obtain)) loop = false
    } while (loop)
    
    r
  }
  
  @inline def release[R <: Referable[R]](r: R) = r.release()
}



