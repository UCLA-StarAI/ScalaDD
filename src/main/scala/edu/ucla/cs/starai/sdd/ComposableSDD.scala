package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.graph.DAG

trait ComposableSDD[N <: ComposableSDD[N]] extends SDD with ComposableCircuit[N]{
  
  self: N =>
    
  /**
   * Shorthand for the type of SDD that we are able to compose
   */
  final type SDDN = ComposableSDD[N] with N
  
  override def vtree: BuilderVTree[N]
  
  def unary_!(): SDDN
  def |(l: Literal): SDDN
  def assign(l: Literal): SDDN
  
  def &&(other: SDDN): SDDN
  def ||(other: SDDN): SDDN
  
}

trait ComposableLeafNode[N <: ComposableSDD[N]] extends SDDLeaf with ComposableSDD[N] {
  self: N =>
}

trait ComposableTrueNode[N <: ComposableSDD[N]] extends TrueNode with ComposableLeafNode[N] {
  
  self: N =>
    
  def unary_!(): SDDN = vtree.buildFalse
  def |(l: Literal): SDDN = (vtree lca l.variable).buildTrue
  def assign(l: Literal): SDDN = (vtree lca l.variable).buildLiteral(l)
  
  def &&(that: SDDN): SDDN = (this.vtree lca that.vtree).build(that)
  def ||(that: SDDN): SDDN = (this.vtree lca that.vtree).buildTrue
  
}


trait ComposableFalseNode[N <: ComposableSDD[N]] extends FalseNode with ComposableLeafNode[N]{
  
  self: N =>
    
  def unary_!(): SDDN = vtree.buildTrue
  def |(l: Literal): SDDN = (vtree lca l.variable).buildFalse
  def assign(l: Literal): SDDN  = (vtree lca l.variable).buildFalse
  
  def &&(that: SDDN): SDDN = (this.vtree lca that.vtree).buildFalse
  def ||(that: SDDN): SDDN = (this.vtree lca that.vtree).build(that)
  
}


trait ComposableLiteralNode[N <: ComposableSDD[N]] extends LiteralNode with ComposableLeafNode[N] {
  
  self: N =>
    
  def unary_!(): SDDN = vtree.buildLiteral(!literal)
  
  def |(l: Literal): SDDN = 
    if(this.literal == l) vtree.buildTrue
    else if(this.literal == !l) vtree.buildFalse
    else (this.vtree lca l.variable).buildLiteral(this.literal)
    
  def assign(l: Literal): SDDN = 
    if(this.literal == l) vtree.buildLiteral(this.literal)
    else if(this.literal == !l) vtree.buildFalse
    else {
      val lca = (this.vtree lca l.variable)
      val x = this.vtree.buildLiteral(this.literal)
      val y = lca.nodeFor(l.variable).buildLiteral(l)
      lca.buildDecomposition(x,y)
    }
    
  def &&(that: SDDN): SDDN = (that assign literal)
      
  def ||(that: SDDN): SDDN = !((!that) assign !literal)
  
}

trait ComposableDecisionNode[N <: ComposableSDD[N]] 
  extends DecisionNode[N] with ComposableSDD[N]{
  
  self: N =>
    
  override def vtree: BuilderVTree[N] with VTreeINode[BuilderVTree[N]]
    
  def falseSub = vtree.vr.buildFalse
  def trueSub = vtree.vr.buildTrue

  def unary_!(): SDDN = vtree.buildPartition(primes, subs.map(!_))

  def |(l: Literal): SDDN = 
    if(vtree.vl contains l.variable) {
      vtree.buildPartition(primes.map(_|l), subs)
    }else if(vtree.vr contains l.variable) {
      vtree.buildPartition(primes, subs.map(_|l))
    } else (vtree lca l.variable).build(this)
  
  def assign(l: Literal): SDDN = 
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

  def &&(that: SDDN): SDDN = that match{
    case leaf: ComposableLeafNode[N] with SDDN @unchecked => leaf && this
    case dec: ComposableDecisionNode[N] with SDDN @unchecked => this && dec
    case _ => throw new IllegalArgumentException(
        s"$that is outside of the intended ComposableSDD hierarchy")
  }
  
  def &&(that: ComposableDecisionNode[N] with SDDN): SDDN = {
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
  protected def conjoinBelow(that: ComposableDecisionNode[N] with SDDN): SDDN = {
    assume(this.vtree contains that.vtree)
    if(this.vtree.vl contains that.vtree){
       this.vtree.buildPartition(!that +: this.primes.map(_ && that), this.falseSub +: this.subs)
    }else{
       this.vtree.buildPartition(this.primes, this.subs.map(_ && that))
    }
  }
  
  def ||(that: SDDN): SDDN = !(!this && !that)
  
}

