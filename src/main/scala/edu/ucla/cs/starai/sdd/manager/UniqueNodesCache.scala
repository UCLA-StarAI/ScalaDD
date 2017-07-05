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

package edu.ucla.cs.starai.sdd.manager

import java.util.concurrent.atomic.AtomicLong

import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheStats
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.sdd.XYDecomposition
import edu.ucla.cs.starai.sdd.DecisionNode

trait UniqueNodesCache[N <: SDD] {
  
  def cacheSize: Long
  
  def getOrBuild(decomp: XYDecomposition[N], build: () => N): N
  
  def register(decision: DecisionNode[N] with N) = {
    val registered = getOrBuild(decision.decomp, () => decision)
    require(registered eq decision, "Cannot register node that is already cached")
  }
  
}

class GoogleWeakCache[N <: SDD] extends UniqueNodesCache[N] {
    
  // make sure cache is symmetric wrt arguments
  // make sure cache keys cannot reference cache values, or auto-GC is broken!
  private[this] case class Key(elems: Set[(N,N)]){

    def this(decomp: XYDecomposition[N]){
      this(decomp.elements.map(e => (e.prime,e.sub)).toSet)
    }
    
  }
  
  private[this] val cache: Cache[Key,N] = CacheBuilder
    .newBuilder
    .weakValues()
    .build()
    
  def getOrBuild(decomp: XYDecomposition[N], build: () => N): N = {
    cache.get(new Key(decomp), build)
  }
    
  def cacheSize: Long = cache.size
  
  // currently does nothing because stats are not kept
  def cacheStats: CacheStats = cache.stats
  
  override def toString = cache.asMap.toString()
  
}
