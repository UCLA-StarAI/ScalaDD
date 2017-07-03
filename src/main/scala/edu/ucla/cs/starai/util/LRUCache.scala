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

import com.google.common.cache.LoadingCache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader

/**
 * Be careful whe keys can reference values: it breaks the weak or soft GC
 */
object LRUCache {

  def apply[Key <: Object, Value <: Object](builder: Key => Value) = new LRUCache1(builder)

  def apply[Key1 <: Object, Key2 <: Object, Value <: Object](builder: (Key1, Key2) => Value) = new LRUCache2(builder)

}

sealed trait LRUCache

class LRUCache1[Key <: Object, Value <: Object](builder: Key => Value) 
  extends LRUCache with (Key => Value) {

  private[this] val cache: LoadingCache[Key, Value] = CacheBuilder.newBuilder
    .softValues()
    .build(
      new CacheLoader[Key, Value]() {
        def load(key: Key): Value = {
          return builder(key);
        }
      })

  def apply(key: Key): Value = cache.get(key)

}

class LRUCache2[Key1 <: Object, Key2 <: Object, Value <: Object](builder: (Key1, Key2) => Value) 
  extends LRUCache  with ((Key1, Key2) => Value) {

  private[this] val cache: LoadingCache[(Key1, Key2), Value] = CacheBuilder.newBuilder
    .softValues()
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

}

