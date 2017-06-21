package edu.ucla.cs.starai

package object util {

   def assertFalse = assert(false);
  
   
  implicit class SetOverlaps[T](val x: Set[T]) {
  
    def overlaps(y: Set[T]): Boolean = {
     (x intersect y).nonEmpty
    }  
    
  }
   
}