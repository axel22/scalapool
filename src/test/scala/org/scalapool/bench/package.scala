/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import singlethread._



package bench {
  
  final class Foo(_x: Int) extends Linkable[Foo] {
    def this() = this(0)
    
    @inline final var x: Int = _x
  }
  
}
