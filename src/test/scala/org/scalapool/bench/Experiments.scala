/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool
package bench



import singlethread._
import scala.testing.Benchmark
import compat.Platform



object OneObject extends MultiMain {
  
  var reference: Foo = null
  
  def run() {
    val sz = size / par
    
    val threads = for (i <- 0 until par) yield new Thread {
      var foo: Foo = null
      override def run() {
        var i = 0
        while (i < sz) {
          foo = allocate()
          reference = foo
          dispose(foo)
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  // pool
  
  var obj: Foo = new Foo
  
  def allocate(): Foo = {
    obj
  }
  
  def dispose(f: Foo) {
    obj = f
  }
  
}


object ObjectArray extends MultiMain {
  
  var reference: Foo = null
  
  def run() {
    val sz = size / par
    
    val threads = for (index <- 0 until par) yield new Thread {
      var foo: Foo = null
      override def run() {
        var i = 0
        val idx = index
        while (i < sz) {
          foo = allocate(idx)
          reference = foo
          dispose(idx, foo)
          i += 1
        }
      }
    }
    
    threads.foreach(_.start())
    threads.foreach(_.join())
  }
  
  // pool
  
  val arr = Array.fill[Foo](par * 128)(new Foo)
  
  def allocate(idx: Int): Foo = {
    arr(idx * 128)
  }
  
  def dispose(idx: Int, f: Foo) {
    arr(idx * 128) = f
  }
  
}


