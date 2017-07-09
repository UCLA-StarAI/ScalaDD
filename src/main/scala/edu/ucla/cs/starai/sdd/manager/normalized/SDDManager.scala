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
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache
import com.google.common.cache.CacheBuilder
import com.google.common.collect.ImmutableMap
import scala.collection._
import scala.collection.immutable.IntMap


trait SDDManager extends VTree[SDDManager] with BuilderVTree[ManagedSDD]{
  
  def kind: Either[SDDManagerLeaf,SDDManagerINode]
  
}

object SDDManager{
  
  def apply(vtree: VTree.Some): SDDManager = SDDManagerImpl(vtree)
  
}

trait SDDManagerLeaf extends SDDManager with VTreeLeaf[SDDManager] {
  
  override def kind = Left(this)
  
  val True: ManagedTerminal = new ManagedTrueTerminal(this)
  val False: ManagedTerminal = new ManagedFalseTerminal(this)
  val posLit: ManagedTerminal = new ManagedLiteralTerminal(this,variable)
  val negLit: ManagedTerminal = new ManagedLiteralTerminal(this,!variable)
  
  def literal(l: Literal) = {
     if(this.variable == l.variable) {
       if(l.isPositive) posLit else negLit
     }else throw new IllegalArgumentException(s"$this cannot build literal $l, only for $variable")
  }
  
  def partition(decomp: XYDecomposition[ManagedSDD]): ManagedSDD = {
    throw new IllegalArgumentException(s"$this cannot build partitions")
  }
  
  def indepConjoin(x: ManagedSDD, y: ManagedSDD): ManagedDecision = {
    throw new IllegalArgumentException(s"$this cannot build decompositions")
  }
  
  def normalize(sdd: ManagedSDD): ManagedSDD = {
    if(sdd.vtree == this) return sdd
    else throw new IllegalArgumentException(s"$this cannot build sdds for other managers")
  }
  
}

trait SDDManagerINode extends SDDManager with VTreeINode[SDDManager] {
  
  override def kind = Right(this)
  
  val uniqueNodesCache: UniqueNodesCache[ManagedSDD]
  override val numVariables = super.numVariables 
  
  private[this] val trimmablePrimes = Seq(vl.True)
  private[this] val trimmableSubs = Seq(vr.True,vr.False)
  
  val True = new ManagedTrueDecision(this,CompressedXYDecomposition(vl.True,vr.True))
  uniqueNodesCache.register(True)
      
  val False = new ManagedFalseDecision(this,CompressedXYDecomposition(vl.True,vr.False))
  uniqueNodesCache.register(False)
     
  private def buildLiteral(l: Literal): ManagedSDD = {
    val v: Variable = l.variable
    if(this.containsVar(v)){
       val decomp = if(vl.containsVar(v))
           CompressedXYDecomposition(vl.literal(l),vr.True,vl.literal(!l),vr.False)
         else
           CompressedXYDecomposition(vl.True,vr.literal(l))
       uniqueNodesCache.getOrBuild(decomp, () => literal_*(decomp,l))
     }else throw new IllegalArgumentException(s"$this does not contain $l")
  }
  
  private[this] val literalCache = 
    IntMap(literals.map(l => (l.toInt -> buildLiteral(l))).toSeq: _*)
  
  override def literal(l: Literal) = {
    assume(literalCache.contains(l.toInt), s"$l is not managed by $this")
    literalCache(l.toInt)
  }
  
  private def literal_*(decomp: CompressedXYDecomposition[ManagedSDD],l: Literal) = 
    new ManagedLiteralDecision(this,decomp,l)
       
  def partition(decomp: XYDecomposition[ManagedSDD]): ManagedSDD = decomp match {
    case decomp: CompressedXYDecomposition[ManagedSDD] => 
      uniqueNodesCache.getOrBuild(decomp, () => partition_*(decomp))
    case _ => ???
  }
  
  private def partition_*(decomp: CompressedXYDecomposition[ManagedSDD]): ManagedSDD = {
    new ManagedComplexDecision(this,decomp)
  }
  
  def indepConjoin(x: ManagedSDD, y: ManagedSDD): ManagedSDD = {
    assume(x.vtree!=this)
    assume(y.vtree!=this)
    assume(this.containsNode(x.vtree))
    assume(this.containsNode(x.vtree))
    val xleft = vl.containsNode(x.vtree) 
    val yleft = vl.containsNode(y.vtree) 
    if(xleft && !yleft){
      partition(CompressedXYDecomposition(x,y,!x,vr.False))
    }else if(!xleft && yleft){
      partition(CompressedXYDecomposition(y,x,!y,vr.False))
    }else if(xleft && yleft){
      normalizeLeft(vl.indepConjoin(x, y))
    }else{
      normalizeRight(vr.indepConjoin(x, y))
    }
  }
  
  def normalize(sdd: ManagedSDD): ManagedSDD = {
    if(sdd.vtree == this) {
      return sdd
    } else if(!this.containsNode(sdd.vtree)){
      throw new IllegalArgumentException(s"$this cannot build sdds for other managers")
    } else if(vl.containsNode(sdd.vtree)){
      normalizeLeft(sdd)
    }else{
      assume(vr.containsNode(sdd.vtree))
      normalizeRight(sdd)
    }
  }
  
  def normalizeLeft(sdd: ManagedSDD) = {
    val sddLeft = vl.normalize(sdd)
    partition(CompressedXYDecomposition(sddLeft,vr.True,!sddLeft,vr.False))
  }
  
  def normalizeRight(sdd: ManagedSDD) = 
      partition(CompressedXYDecomposition(vl.True,vr.normalize(sdd)))
      
}