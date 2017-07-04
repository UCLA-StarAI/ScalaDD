/*
 * Copyright 2017 Guy Van den Broeck
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
import scala.collection._
import scala.language.implicitConversions
 
package object util {

  def assertFalse = assert(false);
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
   
  implicit def runnable(f: () => Unit): Runnable = new Runnable() { def run() = f() }

  implicit def callable[T](f: () => T): Callable[T] = new Callable[T]() { def call() = f() }
  
  implicit final class SetOps[T](val x: Set[T]) {
  
    def overlaps(y: Set[T]): Boolean = {
     (x intersect y).nonEmpty
    }
    
  }
  
  implicit final class SeqOps[T](val x: Seq[T]) {
  
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
    def interleave(y: Seq[T]): Seq[T] = {
      x.zip(y) flatMap { case (a, b) => Seq(a, b) }
    }

    
  }
  
}