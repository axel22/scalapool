/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool.concurrent



import annotation.unchecked._



/** Describes objects which can be linked.
 *  
 *  Linkable objects should extend this trait:
 *  {{{
 *  class Node extends Linkable[Node]
 *  }}}
 */
abstract class Linkable[+R] {
  @inline private[scalapool] var _linkable_next: R @uncheckedVariance = _
}
