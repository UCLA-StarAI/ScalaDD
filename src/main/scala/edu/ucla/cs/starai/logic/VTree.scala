package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.sdd.SDD

trait VTree[+N <: VTree[N]] extends DoubleLinkedTree[N] {
  
  self: N =>
 
  def kind: Either[VTreeLeaf[N],VTreeINode[N]]
    
  def contains(v: Variable): Boolean = variables.contains(v)
  def variables: Set[Variable]
  def literals: Set[Literal] = variables.map(!_.toLiteral) union variables.map(_.toLiteral)
  def numVariables = variables.size
    
  def lca(v: Variable): N = ancestors.find(_.contains(v)).get
  
  /**
   * Assumes there exists an answer.
   */
  def nodeFor(v: Variable): N = find(_ match{
    case x: VTreeLeaf[_] => x.variable == v
    case _ => false
  }).get
  
}

object VTree{
  
  type SomeVtree = VTree[T] forSome { type T <: VTree[T] }
  
  /**
   * Generate balanced vtree for variables X{offset+1} to X{offset+numVars} inclusive
   */
  def balanced(numVars: Int, offset: Int=0): SomeVtree = 
    VTreeImpl.balanced(numVars,offset)
  
}

trait VTreeLeaf[+N <: VTree[N]] extends VTree[N] {
  
  self: N =>
  
  override def children = Seq()
  
  def variable: Variable
  
  def kind = Left(this)
  
  override def contains(v: Variable) = (v == variable)
  def variables: Set[Variable] = Set(variable)
  override def numVariables = 1
  
  
}

trait VTreeINode[+N <: VTree[N]] extends VTree[N] {
  self: N =>

  assume(!(vl.variables overlaps vr.variables), "Variables in left and right branch should be disjoint.")
    
  def vl: VTree[N] with N 
  def vr: VTree[N] with N
  
  def kind = Right(this)
  
  override val  variables = vl.variables union vr.variables
  override val  children = Seq(vl,vr)
  
}
