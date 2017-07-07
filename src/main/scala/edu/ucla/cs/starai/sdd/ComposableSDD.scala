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

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.graph.DAG

/**
 * An SDD that supports logical transformations with other SDDs of the same type
 */
trait ComposableSDD[N <: ComposableSDD[N]] extends SDD with ComposableCircuit[N]{
  
  self: N =>
      
  override def vtree: BuilderVTree[N]
  
  def unary_!(): N
  def |(l: Literal): N
  def assign(l: Literal): N
  
  def &&(other: N): N
  def ||(other: N): N
  
  override def kind: Either[ComposableTerminal[N] with N,ComposableDecisionNode[N] with N]
  
}

trait FastComposable[N <: ComposableSDD[N]] extends ComposableSDD[N]{
  
  self: N =>
        
}

trait ComposableTerminal[N <: ComposableSDD[N]] extends TerminalNode with FastComposable[N]{
  
  self: N =>
    
  override def kind = Left(this)
  
  override def assign(l: Literal): N  = 
    throw new UnsupportedOperationException("Assignment method needs to be overwritten")
  
}

trait ComposableTrueNode[N <: ComposableSDD[N]] extends TrueNode with FastComposable[N]{
  
  self: N =>
    
  // needs to use override everywhere to allow mixing of traits
  override def unary_!(): N = vtree.False
  override def |(l: Literal): N = (vtree lca l.variable).True
  override def assign(l: Literal): N = (vtree lca l.variable).literal(l)
  
  override def &&(that: N): N = (this.vtree lca that.vtree).normalize(that)
  override def ||(that: N): N = (this.vtree lca that.vtree).True
  
}


trait ComposableFalseNode[N <: ComposableSDD[N]] extends FalseNode with FastComposable[N]{
  
  self: N =>
    
  override def unary_!(): N = vtree.True
  override def |(l: Literal): N = (vtree lca l.variable).False
  override def assign(l: Literal): N  = (vtree lca l.variable).False
  
  override def &&(that: N): N = (this.vtree lca that.vtree).False
  override def ||(that: N): N = (this.vtree lca that.vtree).normalize(that)
  
}


trait ComposableLiteralNode[N <: ComposableSDD[N]] extends LiteralNode with FastComposable[N] {
  
  self: N =>
    
  override def unary_!(): N = vtree.literal(!literal)
  
  override def |(l: Literal): N = 
    if(this.literal == l) vtree.True
    else if(this.literal == !l) vtree.False
    else (this.vtree lca l.variable).literal(this.literal)
    
  override abstract def assign(l: Literal): N = 
    if(this.literal == l) this
    else if(this.literal == !l) vtree.False
    else {
      val lca = (this.vtree lca l.variable)
      val Some((c1,c2,parent)) = lca.splitFor(l.variable,this.variable)
      val lit1 = c1.literal(l)
      val lit2 = c2.literal(this.literal)
      lca.normalize(parent.indepConjoin(lit1,lit2))
    }
    
  override def &&(that: N): N = (that assign literal)
      
  override def ||(that: N): N = !((!that) assign !literal)
  
}

trait ComposableDecisionNode[N <: ComposableSDD[N]] extends DecisionNode[N] with ComposableSDD[N] {
  
  self: N =>
       
  override def vtree: BuilderVTree[N] with VTreeINode[BuilderVTree[N]]
    
  def decomp: ComposableXYDecomposition[N]
  
  override def kind = Right(this)

  def unary_!(): N = vtree.partition(!this.decomp)

  def |(l: Literal): N = 
    if(vtree.vl contains l.variable) conditionLeft(l)
    else if(vtree.vr contains l.variable) conditionRight(l)
    else (vtree lca l.variable).normalize(this)
  
  protected def conditionLeft(l: Literal) = vtree.partition(decomp.mapPrimes(_|l))
  protected def conditionRight(l: Literal) =  vtree.partition(decomp.mapSubs(_|l))
    
  def assign(l: Literal): N = 
    if(vtree.vl contains l.variable) assignLeft(l)
    else if(vtree.vr contains l.variable) assignRight(l)
    else {
      val lca = (this.vtree lca l.variable)
      val lit = lca.childFor(l.variable).get.literal(l)
      lca.indepConjoin(this,lit)
    }

  @inline
  protected def assignLeft(l: Literal) = {
    val lit = vtree.vl.literal(l)
    vtree.partition(decomp.mapPrimes(_ assign l) + (!lit, vtree.vr.False))
  }
  
  @inline protected def assignRight(l: Literal) =  vtree.partition(decomp.mapSubs(_ assign l))

  /**
   * If FastComposable, dispatch to other
   */
  // TODO optimize for decompositions of size 1 and 2
  override def &&(that: N): N = {
    assume(!this.isInstanceOf[FastComposable[_]], "FastComposables must override conjoin")
    that.kind match{
      case Left(terminal) => terminal && this
      case Right(decision: FastComposable[N]) => decision && this
      case Right(decision) => this conjoinDecision decision
    }}
      
  protected def conjoinDecision(that: ComposableDecisionNode[N] with N): N = {
    if(this.vtree == that.vtree) vtree.partition(this.decomp && that.decomp)
    else if(this.vtree contains that.vtree) this.conjoinBelow(that)
    else if(that.vtree contains this.vtree) that.conjoinBelow(this)
    else (this.vtree lca that.vtree).indepConjoin(this,that)
  }

  /**
   * Conjoin with an argument that respects a vtree node strictly below this node's vtree
   */
  protected def conjoinBelow(that: ComposableDecisionNode[N] with N): N = {
    if(this.vtree.vl contains that.vtree){
       this.vtree.partition(decomp.mapPrimes(_ && that) + (!that, vtree.vr.False))
    }else{
       assume(this.vtree.vr contains that.vtree)
       this.vtree.partition(decomp.mapSubs(_ && that))
    }
  }
}