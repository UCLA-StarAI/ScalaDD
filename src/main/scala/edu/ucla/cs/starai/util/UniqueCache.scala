package edu.ucla.cs.starai.util

import com.google.common.cache.LoadingCache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader
import com.google.common.cache.CacheStats

/**
 * Be careful when keys can reference values: it breaks the weak or soft GC
 */
object UniqueCache {

  def apply[Key <: Object, Value <: Object](builder: Key => Value) = new UniqueCache1(builder)

  def apply[Key1 <: Object, Key2 <: Object, Value <: Object](builder: (Key1, Key2) => Value) = new UniqueCache2(builder)

}

sealed trait UniqueCache{
  
  def size: Long
  def stats: CacheStats
  
}

class UniqueCache1[Key <: Object, Value <: Object](builder: Key => Value) 
  extends UniqueCache  with (Key => Value) {

  private[this] val cache: LoadingCache[Key, Value] = CacheBuilder.newBuilder
    .weakValues()
//    .recordStats() // incurs overhead
    .build(
      new CacheLoader[Key, Value]() {
        def load(key: Key): Value = {
          return builder(key);
        }
      })

  def apply(key: Key): Value = cache.get(key)

  def size = cache.size()
  def stats = cache.stats()
  
}

class UniqueCache2[Key1 <: Object, Key2 <: Object, Value <: Object](builder: (Key1, Key2) => Value) 
  extends UniqueCache  with ((Key1, Key2) => Value){

  private[this] val cache: LoadingCache[(Key1, Key2), Value] = CacheBuilder.newBuilder
    .weakValues()
//    .recordStats() // incurs overhead
    .build(
      new CacheLoader[(Key1, Key2), Value]() {
        def load(key: (Key1, Key2)): Value = {
          return builder(key._1, key._2);
        }
      })

  def apply(key1: Key1, key2: Key2): Value = cache.get((key1, key2))

  def getAll(keys: Set[(Key1, Key2)]): Set[Value] = {
    import scala.collection.JavaConverters._
    val jIterable = keys.asJava
    cache.getAll(jIterable).values().asScala.toSet
  }

  def size = cache.size()
  def stats = cache.stats()
  
}