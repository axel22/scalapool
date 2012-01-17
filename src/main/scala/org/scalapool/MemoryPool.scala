/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool



import annotation.unchecked._



/** Used to create reclaimable objects of a certain type.
 *  
 *  The memory pool allocates objects from its internal storage or by
 *  allocating additional memory, as needed.
 */
abstract class MemoryPool[R] extends Allocator[R] {
  def special: SpecialInitializer[R]
}


object MemoryPool




