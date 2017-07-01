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
  
}

trait ComposableTrueNode[N <: ComposableSDD[N]] extends TrueNode with FastComposable[N]{
  
  self: N =>
    
  // needs to use override everywhere to allow mixing of traits
  override def unary_!(): N = vtree.buildFalse
  override def |(l: Literal): N = (vtree lca l.variable).buildTrue
  override def assign(l: Literal): N = (vtree lca l.variable).buildLiteral(l)
  
  override def &&(that: N): N = (this.vtree lca that.vtree).decorate(that)
  override def ||(that: N): N = (this.vtree lca that.vtree).buildTrue
  
}


trait ComposableFalseNode[N <: ComposableSDD[N]] extends FalseNode with FastComposable[N]{
  
  self: N =>
    
  override def unary_!(): N = vtree.buildTrue
  override def |(l: Literal): N = (vtree lca l.variable).buildFalse
  override def assign(l: Literal): N  = (vtree lca l.variable).buildFalse
  
  override def &&(that: N): N = (this.vtree lca that.vtree).buildFalse
  override def ||(that: N): N = (this.vtree lca that.vtree).decorate(that)
  
}


trait ComposableLiteralNode[N <: ComposableSDD[N]] extends LiteralNode with FastComposable[N] {
  
  self: N =>
    
  override def unary_!(): N = vtree.buildLiteral(!literal)
  
  override def |(l: Literal): N = 
    if(this.literal == l) vtree.buildTrue
    else if(this.literal == !l) vtree.buildFalse
    else (this.vtree lca l.variable).buildLiteral(this.literal)
    
  override def assign(l: Literal): N = 
    if(this.literal == l) vtree.buildLiteral(this.literal)
    else if(this.literal == !l) vtree.buildFalse
    else {
      val lca = (this.vtree lca l.variable)
      val x = this.vtree.buildLiteral(this.literal)
      val y = lca.nodeFor(l.variable).buildLiteral(l)
      lca.buildDecomposition(x,y)
    }
    
  override def &&(that: N): N = (that assign literal)
      
  override def ||(that: N): N = !((!that) assign !literal)
  
}

trait ComposableDecisionNode[N <: ComposableSDD[N]] 
  extends DecisionNode[N] with ComposableSDD[N]{
  
  self: N =>
    
  override def vtree: BuilderVTree[N] with VTreeINode[BuilderVTree[N]]
    
  override def kind = Right(this)
  
  def falseSub = vtree.vr.buildFalse
  def trueSub = vtree.vr.buildTrue

  def unary_!(): N = vtree.buildPartition(primes, subs.map(!_))

  def |(l: Literal): N = 
    if(vtree.vl contains l.variable) {
      vtree.buildPartition(primes.map(_|l), subs)
    }else if(vtree.vr contains l.variable) {
      vtree.buildPartition(primes, subs.map(_|l))
    } else (vtree lca l.variable).decorate(this)
  
  def assign(l: Literal): N = 
    if(vtree.vl contains l.variable) {
      val lNode = vtree.vl.buildLiteral(l)
      vtree.buildPartition(!lNode +: primes.map(_ assign l), falseSub +: subs)
    }else if(vtree.vr contains l.variable) {
      vtree.buildPartition(primes, subs.map(_ assign l))
    } else {
      val lca = (this.vtree lca l.variable)
      val y = lca.nodeFor(l.variable).buildLiteral(l)
      lca.buildDecomposition(this,y)
    }

  /**
   * If FastComposable, dispatch to other
   */
  // TODO optimize for decompositions of size 1 and 2
  def &&(that: N): N = that.kind match{
    case Left(terminal) => terminal && this
    case Right(decision: FastComposable[N]) => decision && this
    case Right(decision) => this conjoinDecision decision
  }
  
  private[this] def conjoinDecision(that: ComposableDecisionNode[N] with N): N = {
    if(this.vtree == that.vtree){
       val newPrimes = for(x <- this.primes; y <- that.primes) yield (x && y)
       val newSubs = for(x <- this.subs; y <- that.subs) yield (x && y)
       vtree.buildPartition(newPrimes, newSubs)
    }
    else if(this.vtree contains that.vtree) this.conjoinBelow(that)
    else if(that.vtree contains this.vtree) that.conjoinBelow(this)
    else (this.vtree lca that.vtree).buildDecomposition(this,that)
  }

  /**
   * Conjoin with an argument that respects a vtree node strictly below this node's vtree
   */
  private def conjoinBelow(that: ComposableDecisionNode[N] with N): N = {
    assume(this.vtree contains that.vtree)
    if(this.vtree.vl contains that.vtree){
       this.vtree.buildPartition(!that +: this.primes.map(_ && that), this.falseSub +: this.subs)
    }else{
       this.vtree.buildPartition(this.primes, this.subs.map(_ && that))
    }
  }
  
  def ||(that: N): N = !(!this && !that)
  
}

