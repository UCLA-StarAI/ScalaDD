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

package edu.ucla.cs.starai.graph

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce

/**
 * A Traversable that contains itself
 */
trait SelfTraversable[+A] extends Traversable[A]{
  
  // cannot use the toString from TraversableLike: it recurses infinitely if this is part of the Traversable
  override def toString = s"$stringPrefix@$hashCode"
  
  // also need to override flatten, and other undefined operations for SelfTraversables?
  override def flatten[B](implicit asTraversable: A ⇒ GenTraversableOnce[B]): Traversable[B] = 
    throw new UnsupportedOperationException
    
  //hashCode is safe  
  
}