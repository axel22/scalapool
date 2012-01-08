/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool.singlethread



import annotation.unchecked._



/** Describes objects which can be linked. */
trait Linkable[+R <: Linkable[R]] {
  private[mempool] var _linkable_next: R @uncheckedVariance = _
}
