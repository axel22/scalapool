/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org






package object mempool {
  
  /* helpers */
  
  def ??? = throw new UnsupportedOperationException
  
  def illegalarg(msg: String) = throw new IllegalArgumentException(msg)
  
  
  /* combinators */
  
  trait Until[R] {
    def until(cond: =>Boolean): R
  }
  
  def repeat[U](body: =>U) = new Until[Unit] {
    def until(cond: =>Boolean) = while (!cond) body
  }
  
  def aggregate[T](zero: T)(inc: T => T) = new Until[T] {
    def until(cond: =>Boolean) = {
      var sum = zero
      while (!cond) sum = inc(sum)
      sum
    }
  }
  
  def build[S](f: =>S) = new Until[Seq[S]] {
    def until(cond: =>Boolean) = {
      val ab = new collection.mutable.ArrayBuffer[S]
      while (!cond) ab += f
      ab
    }
  }
  
}


