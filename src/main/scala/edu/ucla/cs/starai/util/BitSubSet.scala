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

package edu.ucla.cs.starai.util

import scala.collection.immutable.BitSet
import scala.collection.GenSet

class BitSubSet[T](val backend: Array[T], val elements: BitSet) extends Set[T]{
  
  override def iterator: Iterator[T] = elements.iterator.map { e => backend(e) }   
  
  // move to a sorted list for backend when calling these frequently
  override def - (elem: T): Set[T] = new BitSubSet(backend, elements - (backend.indexOf(elem)))   
  override def + (elem: T): Set[T] = new BitSubSet(backend, elements + (backend.indexOf(elem)))    
  
  override def contains(elem: T): Boolean = elements.contains(backend.indexOf(elem))
  
  override def empty = new BitSubSet(backend,BitSet.empty)
  
  override def filter(p: (T) => Boolean): BitSubSet[T] = {
    new BitSubSet(backend,elements.filter { i => p(backend(i)) })
  }
  
  def intersect(that: BitSubSet[T]): BitSubSet[T] = {
    require(this.backend eq that.backend)
    new BitSubSet(backend, this.elements intersect that.elements)
  }
  
  def union(that: BitSubSet[T]): BitSubSet[T] = {
    require(this.backend eq that.backend)
    new BitSubSet(backend, this.elements union that.elements)
  }
  
  override def size = elements.size
  
  override def toString = mkString("BitSubSet(",",",")")
  
}

object BitSubSet{
  
  def apply[T](backend: Array[T]): BitSubSet[T] = new BitSubSet(backend, BitSet((0 until backend.size):_*))
  
}