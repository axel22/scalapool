/*   ______          _            ___                       *\
**  /_  __/__ ______(_)___ ____  |_  |                      **
**   / /  / // / __/ // _ `/ _ \/ __/                       **
**  /_/   \_, /_/ /_/ \_,_/_//_/____/     Tyrian 2          **
**       /___/                            www.tyrian2.com   **
\*                                        (c) 2011-2012     */

package org.mempool
package util



import org.scalacheck._
import Prop._
import Gen._



object FixedStackSpec extends Properties("FixedStack") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  property("push") = forAll (ranges) {
    range =>
    val fs = new FixedStack[Int](range.length)
    for (n <- range) fs.push(n)
    
    (fs.isFull) :| "is full: %s".format(fs)
    (range.isEmpty || !fs.isEmpty) :| "is non-empty: %s".format(fs) &&
    (range.length == fs.size) :| "sizes are equal: %s".format(fs) &&
    (range.reverse == fs.toBuffer) :| "elements are in reverse order: %s".format(fs)
  }
  
  property("pop") = forAll (ranges) {
    range =>
    val fs = new FixedStack[Int](range.length)
    for (n <- range) fs.push(n)
    val xs = build { fs.pop() } until (fs.isEmpty)
    
    (range.reverse == xs) :| "pops are done in reverse order: %s".format(xs)
  }
  
}
