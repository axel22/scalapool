/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool






/** Describes objects which can be disposed. */
trait Disposable[+R <: Disposable[R]] {

  /** Optionally disposes this object.
   *  
   *  A disposed object may not be used in the application again.
   *  Typically, objects implementing this trait are stored in a
   *  memory pool after `dispose` has been called, so that they
   *  could be reinitialized again without going through a GC cycle.
   *  
   *  If the implementation of the object does not dispose, the object
   *  will still be collected by the GC. This allows for opportunistic
   *  manual memory management, while ensuring correctness.
   *  
   *  Clients that want to extend this interface to create disposable
   *  objects should implement this method by forwarding the call to
   *  the memory pool that creates the object. Typically, this is a
   *  global memory pool.
   */
  def dispose(): Unit
  
}


