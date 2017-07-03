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