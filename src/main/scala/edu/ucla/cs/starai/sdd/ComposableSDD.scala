package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.graph.DAG

trait BuilderVTree extends DoubleLinkedTree[BuilderVTree] with VTree[BuilderVTree] with SDDBuilder{
  
}

trait ComposableSDD extends SDD with Circuit[ComposableSDD]{
  
  override def vtree: BuilderVTree
  
  def unary_!(): ComposableSDD
  def forget(v: Variable) = ((this | v) || (this | !v))
  def |(l: Literal): ComposableSDD
  def assign(l: Literal): ComposableSDD
  
  // Any output will always respect the LCA of the operand vtrees
  def &&(other: ComposableSDD): ComposableSDD
  def ||(other: ComposableSDD): ComposableSDD
  
}

trait ComposableTrueNode extends TrueNode with ComposableSDD {
  
  def unary_!() = vtree.buildFalse
  def |(l: Literal) = (vtree lca l.variable).buildTrue
  def assign(l: Literal) = (vtree lca l.variable).buildLiteral(l)
  
  def &&(that: ComposableSDD)= (this.vtree lca that.vtree).build(that)
  def ||(that: ComposableSDD)= (this.vtree lca that.vtree).buildTrue
  
}


trait ComposableFalseNode extends FalseNode with ComposableSDD{
  
  def unary_!() = vtree.buildTrue
  def |(l: Literal)= (vtree lca l.variable).buildFalse
  def assign(l: Literal) = (vtree lca l.variable).buildFalse
  
  def &&(that: ComposableSDD)= (this.vtree lca that.vtree).buildFalse
  def ||(that: ComposableSDD)= (this.vtree lca that.vtree).build(that)
}


trait ComposableLiteralNode extends LiteralNode with ComposableSDD {
  
  def unary_!() = vtree.buildLiteral(!literal)
  
  def |(l: Literal) = 
    if(this.literal == l) vtree.buildTrue
    else if(this.literal == !l) vtree.buildFalse
    else (this.vtree lca l.variable).buildLiteral(this.literal)
    
  def assign(l: Literal) = 
    if(this.literal == l) vtree.buildLiteral(this.literal)
    else if(this.literal == !l) vtree.buildFalse
    else {
      val lca = (this.vtree lca l.variable)
      val lvtree = lca.nodeFor(l.variable)
      lca.buildElement(this.vtree.buildLiteral(this.literal),lvtree.buildLiteral(l))
    }
    
  def &&(that: ComposableSDD) = (that assign literal)
      
  def ||(that: ComposableSDD) = !((!that) assign !literal)
  
}

trait ComposableINode extends SDDINode with ComposableSDD


trait ComposableElementNode extends ElementNode with ComposableINode{
  
  override def vtree: BuilderVTree with VTreeINode[BuilderVTree]
  
  def prime: ComposableSDD
  
  def sub: ComposableSDD
  
}
  
  
trait ComposableDecisionNode extends DecisionNode with ComposableINode

