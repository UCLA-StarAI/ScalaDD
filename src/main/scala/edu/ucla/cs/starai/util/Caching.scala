package edu.ucla.cs.starai.util

import scala.collection.mutable

  
/**
 * Mixin for classes that want to provide cache arguments instead of lazy vals.
 * Avoids boilerplate related to typing the self class in the map.
 */
trait Caching[T]{
 
 self: T =>
  
 type Cache[O] = mutable.Map[T,O]
 type Cache1[I,O] = mutable.Map[(T,I),O]
 
 def emptyCache[O]: Cache[O] = mutable.Map.empty[T,O]
 def emptyCache1[I,O]: Cache1[I,O] = mutable.Map.empty[(T,I),O]
 
}