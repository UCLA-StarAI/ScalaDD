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
      val c1 = this.vtree.buildLiteral(this.literal)
      val lvtree = lca.nodeFor(l.variable)
      val c2 = lvtree.buildLiteral(l)
      type VI = BuilderVTree[N] with VTreeINode[BuilderVTree[N]]
      val lca = (this.vtree lca l.variable).asInstanceOf[VI]
      if(lca.vl.contains(this.variable)) {
        lca.buildDecision(Seq(c1,!c1),Seq(c2,lca.vr.buildFalse()))
      }else{
        lca.buildDecision(Seq(c2,!c2),Seq(c1,lca.vr.buildFalse()))
      }
    }
    
  def &&(that: SDDN): SDDN = (that assign literal)
      
  def ||(that: SDDN): SDDN = !((!that) assign !literal)
  
}

trait ComposableDecisionNode[N <: ComposableSDD[N]] extends DecisionNode[N] with ComposableSDD[N]{
  
  self: N =>
    
  override def vtree: BuilderVTree[N] with VTreeINode[_]
    
  def elems: Seq[ComposableElementNode[SDDN] with SDDN]
  override def children: Seq[ComposableElementNode[SDDN] with SDDN] = elems
   
  def &&(that: SDDN): SDDN = that match{
    case leaf: ComposableLeafNode[N] with SDDN @unchecked => leaf && this
    case dec: ComposableDecisionNode[N] with SDDN @unchecked => this && dec
    case _ => throw new IllegalArgumentException(s"$that is outside of the intended ComposableSDD hierarchy")
  }
  
  def &&(that: ComposableDecisionNode[N] with SDDN): SDDN
  
}

