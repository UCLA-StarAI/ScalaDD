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