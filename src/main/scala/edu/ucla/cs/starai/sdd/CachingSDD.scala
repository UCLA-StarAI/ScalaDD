/*
 * Copyright 2017 Guy Van den Broeck <guyvdb@cs.ucla.edu>
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

package edu.ucla.cs.starai.sdd

import com.google.common.cache.CacheBuilder
import edu.ucla.cs.starai.sdd.manager.normalized.ManagedSDD
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.manager.normalized.ManagedDecision
import com.google.common.cache.CacheLoader

trait CachingNegation[N <: ComposableCircuit[N]] extends ComposableCircuit[N] {
  
  self: N =>
    
  override abstract lazy val unary_! = unary_!*
  
  protected def unary_!* = super.unary_!
  
}


trait CachingAssign[N <: ComposableCircuit[N]] extends ComposableCircuit[N] {
  
  self: N =>
    
  private[this] val assignCache = CachingAssign.cacheBuilder.build[Integer,N]()
  
  override abstract def assign(l: Literal): N = {
    assignCache.get(l.toInt,() => assign_*(l))
  }
  
  protected def assign_*(l: Literal):N = super.assign(l)
  
}

object CachingAssign{
  
  val cacheBuilder = CacheBuilder
    .newBuilder
    .softValues
    .concurrencyLevel(1)
    .initialCapacity(128)
  
}

trait CachingConjoinDecision[N <: ComposableSDD[N]] extends ComposableDecisionNode[N] {
  
  self: N =>
    
  private[this] val andCache = CachingConjoinDecision.cacheBuilder.build(
    new CacheLoader[ComposableDecisionNode[N] with N,N](){
      def load(that: ComposableDecisionNode[N] with N) = conjoinDecision_*(that)
    })
    
  override protected def conjoinDecision(that: ComposableDecisionNode[N] with N): N = {
    andCache.get(that)
  }
  
  protected def conjoinDecision_*(that: ComposableDecisionNode[N] with N): N = {
    super.conjoinDecision(that)
  }
  
}

object CachingConjoinDecision{
  
  val cacheBuilder = CacheBuilder
    .newBuilder
    .weakKeys
    .softValues
    .concurrencyLevel(1)
    .initialCapacity(128)
  
}


