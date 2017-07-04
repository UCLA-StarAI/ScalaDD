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

import edu.ucla.cs.starai.sdd.BuilderVTree
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.Compressed
import edu.ucla.cs.starai.sdd.Normalized
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.TerminalNode
import edu.ucla.cs.starai.sdd.DecisionNode
import edu.ucla.cs.starai.sdd.TrueNode
import edu.ucla.cs.starai.sdd.FalseNode
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache


trait SDDManager extends VTree[SDDManager] with BuilderVTree[ManagedSDD]{
  
  def kind: Either[SDDManagerLeaf,SDDManagerINode]
  
}

object SDDManager{
  
  def apply(vtree: VTree.SomeVtree): SDDManager = SDDManagerImpl(vtree)
  
}

trait SDDManagerLeaf extends SDDManager with VTreeLeaf[SDDManager] {
  
  override def kind = Left(this)
  
  private abstract class MyTerminal extends {
    val vtree = SDDManagerLeaf.this
  } with  ManagedTerminal
  
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
  
  def partition(primes: Seq[ManagedSDD],subs: Seq[ManagedSDD]): ManagedDecision = {
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
  
  private class MyDecision(val primes: Seq[ManagedSDD], val subs: Seq[ManagedSDD]) 
    extends {
    val vtree = SDDManagerINode.this
  } with ManagedDecision 
  
  private[this] val trimmablePrimes = Seq(vl.True)
  private[this] val trimmableSubs = Seq(vr.True,vr.False)
  
  val True: ManagedDecision with ManagedTrue = 
    new MyDecision(trimmablePrimes,Seq(vr.True)) 
      with ManagedTrue with CachedNegation
  uniqueNodesCache.register(True.primes, True.subs, True)
      
  val False: ManagedDecision with ManagedFalse = 
    new MyDecision(trimmablePrimes,Seq(vr.False)) 
      with ManagedFalse with CachedNegation
  uniqueNodesCache.register(False.primes, False.subs, False)
      
  override def literal(l: Literal) = literalCache(l)
      
  protected val literalCache: Map[Literal,ManagedSDD] = (
       vl.literals.map{ l => 
         val primes = Seq(vl.literal(l),!vl.literal(l))
         (l-> uniqueNodesCache.getOrBuild(primes, trimmableSubs,
           () => new MyDecision(primes,trimmableSubs) 
                   with ManagedLiteral with CachedNegation {
             def literal = l
           }))
       } ++ 
       vr.literals.map{ l => 
         val subs = Seq(vr.literal(l))
         (l->uniqueNodesCache.getOrBuild(trimmablePrimes, subs,
           () => new MyDecision(trimmablePrimes,subs) 
                   with ManagedLiteral with CachedNegation {
             def literal = l
           }))
       }).toMap
  
  def partition(primes: Seq[ManagedSDD], subs: Seq[ManagedSDD]): ManagedSDD = {
    val normalizedPrimes = primes.map(vl.normalize(_))
    val normalizedSubs = subs.map(vr.normalize(_))
    val compressed = normalizedSubs.hasDistinctElements
    val consistentPrimes = primes.forall(_.isConsistent)
    val (newPrimes,newSubs) = 
      if(compressed && consistentPrimes) (normalizedPrimes,normalizedSubs)
      else {
        // remove inconsistent primes
        val elems = (normalizedPrimes zip normalizedSubs).filter(_._1.isConsistent)
        // compress
        val primesBySub = elems.groupBy(_._2 ).mapValues { _.map( _._1) }
    	  val primeBySub = primesBySub.mapValues { _.reduce { (p1, p2) => p1 || p2 } 
  	                                              /*TODO: optimize this order*/ }
    	  val (subsIter,primesIter) = primeBySub.unzip
    	  (primesIter.toSeq,subsIter.toSeq)
      }
     uniqueNodesCache.getOrBuild(newPrimes, newSubs, () => 
       new MyDecision(newPrimes,newSubs))
  }
  
  def indepConjoin(x: ManagedSDD, y: ManagedSDD): ManagedSDD = {
    assume(x.vtree!=this)
    assume(y.vtree!=this)
    assume(this.contains(x.vtree))
    assume(this.contains(x.vtree))
    val xleft = vl.contains(x.vtree) 
    val yleft = vl.contains(y.vtree) 
    if(xleft && !yleft){
      partition(Seq(x,!x),Seq(y,vr.False))
    }else if(!xleft && yleft){
      partition(Seq(y,!y),Seq(x,vr.False))
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
  
  def decorateLeft(sdd: ManagedSDD) = partition(Seq(sdd,!sdd),trimmableSubs)
  
  def decorateRight(sdd: ManagedSDD) = partition(trimmablePrimes,Seq(sdd))
  
}