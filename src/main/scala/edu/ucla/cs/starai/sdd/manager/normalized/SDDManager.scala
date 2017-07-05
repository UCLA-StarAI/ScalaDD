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
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache


trait SDDManager extends VTree[SDDManager] with BuilderVTree[ManagedSDD]{
  
  def kind: Either[SDDManagerLeaf,SDDManagerINode]
  
}

object SDDManager{
  
  def apply(vtree: VTree.Some): SDDManager = SDDManagerImpl(vtree)
  
}

trait SDDManagerLeaf extends SDDManager with VTreeLeaf[SDDManager] {
  
  override def kind = Left(this)
  
  private abstract class MyTerminal extends {
    val vtree = SDDManagerLeaf.this
  } with ManagedTerminal
  
  private abstract class MyLiteral(val literal: Literal) extends MyTerminal
  
  val True: ManagedTerminal = new MyTerminal with ManagedTrue
  val False: ManagedTerminal = new MyTerminal with ManagedFalse
  val posLit: ManagedTerminal = new MyLiteral(variable) with ManagedLiteral
  val negLit: ManagedTerminal = new MyLiteral(!variable) with ManagedLiteral
  
  def literal(l: Literal) = {
     if(this.variable == l.variable) {
       if(l.isPositive) posLit
       else negLit
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
  
  private class MyDecision(val decomp: CompressedXYDecomposition[ManagedSDD]) 
    extends {
    val vtree = SDDManagerINode.this
  } with ManagedDecision 
  
  private[this] val trimmablePrimes = Seq(vl.True)
  private[this] val trimmableSubs = Seq(vr.True,vr.False)
  
  val True: ManagedDecision with ManagedTrue = 
    new MyDecision(CompressedXYDecomposition(vl.True,vr.True)) 
      with ManagedTrue with CachedNegation
  uniqueNodesCache.register(True)
      
  val False: ManagedDecision with ManagedFalse = 
    new MyDecision(CompressedXYDecomposition(vl.True,vr.False)) 
      with ManagedFalse with CachedNegation
  uniqueNodesCache.register(False)
      
  override def literal(l: Literal) = literalCache(l)
      
  protected val literalCache: Map[Literal,ManagedSDD] = (
       vl.literals.map{ l => 
         val decomp = CompressedXYDecomposition(vl.literal(l),vr.True,vl.literal(!l),vr.False)
         (l-> uniqueNodesCache.getOrBuild(decomp,
           () => new MyDecision(decomp) 
                   with ManagedLiteral with CachedNegation {
             def literal = l
           }))
       } ++ 
       vr.literals.map{ l => 
         val decomp = CompressedXYDecomposition(vl.True,vr.literal(l))
         (l->uniqueNodesCache.getOrBuild(decomp,
           () => new MyDecision(decomp) 
                   with ManagedLiteral with CachedNegation {
             def literal = l
           }))
       }).toMap
  
  def partition(decomp: XYDecomposition[ManagedSDD]): ManagedSDD = decomp match {
    case decomp: CompressedXYDecomposition[ManagedSDD] => 
      uniqueNodesCache.getOrBuild(decomp, () => new MyDecision(decomp))
    case _ => ???
  }
  
  def indepConjoin(x: ManagedSDD, y: ManagedSDD): ManagedSDD = {
    assume(x.vtree!=this)
    assume(y.vtree!=this)
    assume(this.contains(x.vtree))
    assume(this.contains(x.vtree))
    val xleft = vl.contains(x.vtree) 
    val yleft = vl.contains(y.vtree) 
    if(xleft && !yleft){
      partition(CompressedXYDecomposition(x,y,!x,vr.False))
    }else if(!xleft && yleft){
      partition(CompressedXYDecomposition(y,x,!y,vr.False))
    }else if(xleft && yleft){
      decorateLeft(vl.indepConjoin(x, y))
    }else{
      decorateRight(vr.indepConjoin(x, y))
    }
  }
  
  def normalize(sdd: ManagedSDD): ManagedSDD = {
    if(sdd.vtree == this) {
      return sdd
    } else if(!this.contains(sdd.vtree)){
      throw new IllegalArgumentException(s"$this cannot build sdds for other managers")
    } else if(vl.contains(sdd.vtree)){
      decorateLeft(sdd)
    }else{
      assume(vr.contains(sdd.vtree))
      decorateRight(sdd)
    }
  }
  
  def decorateLeft(sdd: ManagedSDD) = 
      partition(CompressedXYDecomposition(sdd,vr.True,!sdd,vr.False))
  
  def decorateRight(sdd: ManagedSDD) = 
      partition(CompressedXYDecomposition(vl.True,sdd))
      
}