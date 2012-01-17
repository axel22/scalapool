/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
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
    val fs = new FixedStack[java.lang.Integer](range.length)
    for (n <- range) fs.push(n)
    
    (fs.isFull) :| "is full: %s".format(fs)
    (range.isEmpty || !fs.isEmpty) :| "is non-empty: %s".format(fs) &&
    (range.length == fs.size) :| "sizes are equal: %s".format(fs) &&
    (range.reverse == fs.toBuffer) :| "elements are in reverse order: %s".format(fs)
  }
  
  property("pop") = forAll (ranges) {
    range =>
    val fs = new FixedStack[java.lang.Integer](range.length)
    for (n <- range) fs.push(n)
    val xs = build { fs.pop() } until (fs.isEmpty)
    
    (range.reverse == xs) :| "pops are done in reverse order: %s".format(xs)
  }
  
}
