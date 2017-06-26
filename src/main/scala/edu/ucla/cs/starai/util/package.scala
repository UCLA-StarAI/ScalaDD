package edu.ucla.cs.starai

import scala.collection.mutable

package object util {

  def assertFalse = assert(false);
   
  implicit class SetOps[T](val x: Set[T]) {
  
    def overlaps(y: Set[T]): Boolean = {
     (x intersect y).nonEmpty
    }
    
  }
  
  implicit class SeqOps[T](val x: Seq[T]) {
  
    def distinctElements: Boolean =  x.toSet.size == x.size
    
    def interleave(y: Seq[T]): Seq[T] = {
      x.zip(y) flatMap { case (a, b) => Seq(a, b) }
    }

    
  }
  
}