//package edu.ucla.cs.starai.util
//
//import scala.annotation._
//import scala.annotation.elidable._
//
///**
// * A class that defines a stacked set of 
// * assertions that can only be run after the object is fully constructed
// */
//trait Assumes {
//  
//  @elidable(ASSERTION) @inline
//  def assumes {}
//  
//  // Enforcing Closed-ness with a self-type is tedious 
//  // because it requires repeating the self-type everywhere.
//  // Hence we enforce it at runtime.
//  
//  def isClosed {
//    throw new IllegalStateException("This Assertion object does not inherit Closed")
//  }
//  
//  // check if this Assertions object has been closed
//  isClosed
//  
//}
//
///**
// * A fully constructed Assertions object
// */
//trait Closed extends Assumes{
//  
//  this.assumes
//  
//  final override def isClosed {
//    // no op
//  }
//  
//}