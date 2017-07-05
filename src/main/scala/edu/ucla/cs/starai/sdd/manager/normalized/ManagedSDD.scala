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

package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd._
import scala.collection._

/**
 * A normalized compressed SDD that is managed by its VTree
 */
sealed trait ManagedSDD extends Circuit[ManagedSDD]
  with ComposableSDD[ManagedSDD] 
  with Normalized with Compressed[ManagedSDD]{
  
  def vtree: SDDManager
  
    // this seems to speed things up slightly for no reason
  final override def equals(that: Any): Boolean = (that.asInstanceOf[AnyRef] eq this)
  
  // gets called thousands of times
  final override def hashCode = System.identityHashCode(this);
  
}

trait ManagedDecision extends ManagedSDD
  with ComposableDecisionNode[ManagedSDD] 
  with NormalizedDecision[ManagedSDD] with CompressedDecision[ManagedSDD] {
  
  def vtree: SDDManagerINode
  
  override def decomp: CompressedXYDecomposition[ManagedSDD]
  
}

trait ManagedTerminal extends ManagedSDD with ComposableTerminal[ManagedSDD] with NormalizedTerminal {
    
  def vtree: SDDManagerLeaf
  
}

trait ManagedTrue extends ManagedSDD with ComposableTrueNode[ManagedSDD]
trait ManagedFalse extends ManagedSDD with ComposableFalseNode[ManagedSDD]
trait ManagedLiteral extends ManagedSDD with ComposableLiteralNode[ManagedSDD]{
  
}

trait ManagedDecisionLiteral extends ManagedDecision with ManagedLiteral{
  
  // specialized for performance improvement
  override def assign(l: Literal): ManagedSDD = {
    if(this.literal == l) this
    else if(this.literal == !l) vtree.False
    else if(vtree.vl contains l.variable) assignLeft(l)
    else if(vtree.vr contains l.variable) assignRight(l)
    else {
      val lca = (this.vtree lca l.variable)
      val y = lca.nodeFor(l.variable).literal(l)
      lca.indepConjoin(this,y)
    }
  }
  
  // specialized for performance improvement
  override def &&(that: ManagedSDD): ManagedSDD = (that assign literal)
  
}

trait CachedNegation extends ManagedSDD {
  
    override abstract lazy val unary_! = super.unary_!
    
}