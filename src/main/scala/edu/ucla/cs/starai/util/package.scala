/*
 * Copyright 2017 Guy Van den Broeck <guyvdb@cs.ucla.edu>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai

import java.util.concurrent.Callable
import scala.annotation.elidable._
import scala.collection._
import scala.language.implicitConversions
import scala.annotation.elidable
 
package object util {

  @elidable(ASSERTION) @inline
  def assumes[T](collection: Traversable[T], assumption: T => Boolean, message: T => Any) {
   for(x <- collection) assume(assumption(x),message(x)) 
  }
  
  @elidable(ASSERTION) @inline
  def assumes[T](collection: Traversable[T], assumption: T => Boolean) {
   assumes(collection,assumption,(x: T) => x.toString) 
  }
  
  def assertFalse = assert(false, "Assertions are enabled");
  
  def time[R](task: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(task + " took " + (t1 - t0)/1000.0 + "s")
    result
  }
   
  implicit def runnable(f: () => Unit): Runnable = new Runnable() { def run() = f() }

  implicit def callable[T](f: () => T): Callable[T] = new Callable[T]() { def call() = f() }
  
  implicit final class SetOps[T](val x: Set[T]) {
  
    def overlaps(y: Set[T]): Boolean = {
     (x intersect y).nonEmpty
    }
    
  }
  
  implicit final class IterableOps[T](val x: Iterable[T]) {
  
    @inline
    def hasDistinctElements: Boolean = {
      val seen = mutable.HashSet[T]()
      for (e <- x) {
        if (!seen(e)) {
          seen += e
        }else{
          return false
        }
      }
      return true
    }
    
    @inline
    def interleave(y: Iterable[T]): Iterable[T] = {
      x.zip(y) flatMap { case (a, b) => Seq(a, b) }
    }

    def nthMaxBy[B](n:Int,f: T => B)(implicit cmp: Ordering[B]): T = {
      if(x.isEmpty) throw new IllegalArgumentException
      val max = x.maxBy(f)(cmp)
      if(n == 0) max
      else x.filter(_ != max).nthMaxBy(n-1,f)(cmp)
    }
    
  }
  
}