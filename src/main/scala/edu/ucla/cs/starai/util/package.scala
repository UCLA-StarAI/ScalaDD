package edu.ucla.cs.starai

import java.util.concurrent.Callable

package object util {

  def assertFalse = assert(false);
   
  implicit def runnable(f: () => Unit): Runnable = new Runnable() { def run() = f() }

  implicit def callable[T](f: () => T): Callable[T] = new Callable[T]() { def call() = f() }
  
  implicit class SetOps[T](val x: Set[T]) {
  
    def overlaps(y: Set[T]): Boolean = {
     (x intersect y).nonEmpty
    }
    
  }
  
  implicit class SeqOps[T](val x: Seq[T]) {
  
    def hasDistinctElements: Boolean =  x.toSet.size == x.size
    
    def interleave(y: Seq[T]): Seq[T] = {
      x.zip(y) flatMap { case (a, b) => Seq(a, b) }
    }

    
  }
  
}