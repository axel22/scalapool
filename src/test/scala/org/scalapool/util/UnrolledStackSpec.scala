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



object UnrolledStackSpec extends Properties("UnrolledStack") {
  
  def ranges = for {
    length <- choose(0, 1000)
  } yield 0 until length
  
  property("push") = forAll (ranges) {
    range =>
    val us = new UnrolledStack[java.lang.Integer]
    for (n <- range) us.push(n)
    
    (range.isEmpty || !us.isEmpty) :| "is non-empty: %s".format(us) &&
    (range.length == us.size) :| "sizes are equal: %s".format(us) &&
    (range.reverse == us.toBuffer) :| "elements are in reverse order: %s".format(us)
  }
  
  property("pop") = forAll (ranges) {
    range =>
    val us = new UnrolledStack[java.lang.Integer]
    for (n <- range) us.push(n)
    val xs = build { us.pop() } until (us.isEmpty)
    
    (range.reverse == xs) :| "pops are done in reverse order"
  }
  
}
