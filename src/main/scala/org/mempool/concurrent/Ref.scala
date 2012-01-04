/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._
import annotation.{tailrec, implicitNotFound}
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater



/** `Ref`s can reference concurrently acquirable objects.
 *  
 *  Using concurrent `Ref`s carries a price in the form of
 *  an indirection.
 *  
 *  Here is a use-case example:
 *  
 *  {{{
 *  class Node extends Acquirable[Node]
 *  val r1 = Ref[Node](new Node) // refers to a node
 *  val r2 = Ref[Node]           // refers to nothing
 *  r2 << r1
 *  // now both refer to the same node
 *  }}}
 *  
 *  It is also possible to assign acquirable objects to a `Ref`.
 *  Since the `:=` is marked with `@inline`, this should not create extra objects.
 *  
 *  {{{
 *  val r1 = Ref[Node]
 *  r1 := new Node
 *  }}}
 */
final class Ref[R <: Acquirable[R]](rawinit: =>R) extends JRef[R] {
  this := rawinit
  
  def this() = this(null.asInstanceOf[R])
  
  @tailrec
  @implicitNotFound(msg = "This is a private method.")
  def reassign(nv: R)(implicit privatemethod: Nothing) {
    val ov = /*READ*/rawref
    if (JRef.updater.compareAndSet(this, ov, nv)) {
      if (ov ne null) release(ov)
    } else reassign(nv)
  }
  
  def apply(): R = rawref
  
  @inline def <<(other: Ref[R]) {
    val r = acquire { other() }
    reassign(r)(null.asInstanceOf[Nothing])
  }
  
  @inline def :=(expr: =>R) {
    val r = acquire { expr }
    reassign(r)(null.asInstanceOf[Nothing])
  }
  
  def clear() {
    reassign(null.asInstanceOf[R])(null.asInstanceOf[Nothing])
  }
  
}


object Ref {
  @inline def apply[R <: Acquirable[R]](rawinit: =>R) = new Ref(rawinit)
  @inline def apply[R <: Acquirable[R]]() = new Ref[R]()
}



