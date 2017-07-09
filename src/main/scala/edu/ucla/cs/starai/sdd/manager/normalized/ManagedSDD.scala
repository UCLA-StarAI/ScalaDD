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

package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd._
import scala.collection._
import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheStats
import com.google.common.cache.CacheLoader

/**
 * A normalized compressed SDD that is managed by its VTree
 */
sealed trait ManagedSDD extends Circuit[ManagedSDD]
  with ComposableSDD[ManagedSDD] 
  with Normalized with Compressed[ManagedSDD]{
  
  def vtree: SDDManager
  
}

sealed abstract class ManagedTerminal extends ManagedSDD with ComposableTerminal[ManagedSDD] with NormalizedTerminal {
    
  def vtree: SDDManagerLeaf
  
}

sealed abstract class ManagedDecision extends ManagedSDD
  with ComposableDecisionNode[ManagedSDD] 
  with NormalizedDecision[ManagedSDD] with CompressedDecision[ManagedSDD] {
  
  def vtree: SDDManagerINode
  def decomp: CompressedXYDecomposition[ManagedSDD]
  
}

// concrete classes

final class ManagedTrueTerminal(val vtree: SDDManagerLeaf) 
  extends ManagedTerminal with ComposableTrueNode[ManagedSDD]{
  
}

final class ManagedFalseTerminal(val vtree: SDDManagerLeaf) 
  extends ManagedTerminal with ComposableFalseNode[ManagedSDD]{
  
}

final class ManagedLiteralTerminal(
    val vtree: SDDManagerLeaf, val literal: Literal) 
  extends ManagedTerminal with ComposableLiteralNode[ManagedSDD]{
  
}

final class ManagedTrueDecision(
    val vtree: SDDManagerINode, 
    val decomp: CompressedXYDecomposition[ManagedSDD]) 
  extends ManagedDecision with ComposableTrueNode[ManagedSDD]{
  
}

final class ManagedFalseDecision(
    val vtree: SDDManagerINode, 
    val decomp: CompressedXYDecomposition[ManagedSDD]) 
  extends ManagedDecision with ComposableFalseNode[ManagedSDD]
  with CachingNegation[ManagedSDD]{
  
  override protected def unary_!* = super[ComposableFalseNode].unary_!
  
}

final class ManagedLiteralDecision(
    val vtree: SDDManagerINode, 
    val decomp: CompressedXYDecomposition[ManagedSDD],
    val literal: Literal) 
  extends ManagedDecision with ComposableLiteralNode[ManagedSDD]
  with CachingAssign[ManagedSDD]
  with CachingNegation[ManagedSDD]{
  
  override protected def unary_!* = super[ComposableLiteralNode].unary_!
  
  override def assign(l: Literal): ManagedSDD = 
    if(this.literal == l) this
    else if(this.literal == !l) vtree.False
    else super[CachingAssign].assign(l)
    
  override protected def assign_*(l: Literal): ManagedSDD = 
    super[ComposableLiteralNode].assign(l)
  
  // caching conjoin here slows things down!
    
}

final class ManagedComplexDecision(
    val vtree: SDDManagerINode, 
    val decomp: CompressedXYDecomposition[ManagedSDD]) 
  extends ManagedDecision
  with CachingAssign[ManagedSDD]
  with CachingNegation[ManagedSDD]
  with CachingConjoinDecision[ManagedSDD]{
  
  // all decisions of this type are consistent by virtue of the unique nodes cache
  override def isConsistent = true
  
  
}
