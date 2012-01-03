/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool
package concurrent



import annotation.unchecked._
import annotation.{tailrec, switch}



/** Describes objects which can be read concurrently, without fear that the object
 *  was disposed during the read.
 *  
 *  No side-effects can be performed while reading a readable object, nor should its
 *  fields be written to.
 *  
 *  Here is an example:
 *  
 *  {{{
 *  class Node(var x: Int) extends Readable[Node]
 *  reading(obtainReference()) {
 *    n => n.x
 *  }
 *  }}}
 *  
 *  Internally, readable objects keep a stamp. An object can be read only if
 *  the stamp is even. If the stamp changes between the start and the end of
 *  the read, that means that the read has to be repeated - somebody must have
 *  recycled the object.
 *  
 *  The `reading` block must not fail with an exception due to reading a
 *  `Readable` object in an inconsistent state.
 */
abstract class Readable[R <: Readable[R]] extends Poolable[R] {
  @volatile
  private var stampcount = -1L
  
  private[mempool] def incrementStamp() {
    stampcount = stampcount + 1
  }
  
  def stamp: Long = stampcount
  
  final override def dispose() {
    incrementStamp()
    super.dispose()
  }
  
}

