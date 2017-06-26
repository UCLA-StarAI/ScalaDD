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
  
  // Any output will always respect the LCA of the operand vtrees
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
      val lvtree = lca.nodeFor(l.variable)
      lca.buildElement(this.vtree.buildLiteral(this.literal),lvtree.buildLiteral(l))
    }
    
  def &&(that: SDDN): SDDN = (that assign literal)
      
  def ||(that: SDDN): SDDN = !((!that) assign !literal)
  
}

trait ComposableINode[N <: ComposableSDD[N]] extends SDDINode with ComposableSDD[N]{
  
  self: N =>
    
  override def vtree: BuilderVTree[N] with VTreeINode[_]
  
}


trait ComposableElementNode[N <: ComposableSDD[N]] extends ElementNode with ComposableINode[N]{
  
  self: N =>
    
  override def children: Seq[SDDN] = Seq(prime,sub)
      
  def prime: SDDN
  def sub: SDDN
  
  def unary_!(): SDDN = vtree.buildDecision(Seq(
      vtree.buildElement(prime, !sub),
      vtree.buildElement(!prime, vtree.buildTrue)))
      
  def |(l: Literal): SDDN = 
    if(this.vl.contains(l.variable)) vtree.buildElement(prime | l, sub)
    else if(this.vr.contains(l.variable)) vtree.buildElement(prime, sub | l)
    else vtree.build(this) // SDD is unaffected by conditioning
  
  def assign(l: Literal): SDDN = 
    if(this.vl.contains(l.variable)) vtree.buildElement(prime assign l, sub)
    else if(this.vr.contains(l.variable)) vtree.buildElement(prime, sub assign l)
    else {
      val lca = (this.vtree lca l.variable)
      val lvtree = lca.nodeFor(l.variable)
      lca.buildElement(this,lvtree.buildLiteral(l))
    }
  
  def &&(that: ComposableElementNode[SDDN] with SDDN): ElementNode with SDDN = {
    if(this.vtree == that.vtree) vtree.buildElement(this.prime && that.prime, this.sub && that.sub)
    else if (this.vtree.vl contains that.vtree) this.vtree.buildElement(this.prime && that, this.sub)
    else if (this.vtree.vr contains that.vtree) this.vtree.buildElement(this.prime, this.sub && that)
    else if (that.vtree.vl contains this.vtree) that.vtree.buildElement(that.prime && this, that.sub)
    else if (that.vtree.vr contains this.vtree) that.vtree.buildElement(that.prime, that.sub && this)
    else (this.vtree lca that.vtree).buildElement(this, that)
  }
  
  def &&(that: ComposableDecisionNode[SDDN] with SDDN): DecisionNode with SDDN = {
    val lca = (this.vtree lca that.vtree)
    val posElems:Seq[ElementNode with SDDN] = that.elems.map(_ && this)
    val extraNegElems = that.elems
  }
  
  def &&(that: SDDN): SDDN = that match{
    case leaf: ComposableLeafNode[N] with SDDN @unchecked => leaf && this
    case elem: ComposableElementNode[N] with SDDN @unchecked => this && elem
    case dec: ComposableDecisionNode[N] with SDDN @unchecked => this && dec
    case _ => throw new IllegalArgumentException(s"$that is outside of the intended ComposableSDD hierarchy")
  }
  
  
  
}
  
  
trait ComposableDecisionNode[N <: ComposableSDD[N]] extends DecisionNode with ComposableINode[N]{
  
  self: N =>
    
  def elems: Seq[ElementNode with SDDN]
  override def children: Seq[ElementNode with SDDN] = elems
    
  def &&(that: ComposableElementNode[SDDN] with SDDN): DecisionNode with SDDN 
}

