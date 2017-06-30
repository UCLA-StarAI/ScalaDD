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
  
  /**
   * Generate balanced vtree for variables X{offset+1} to X{offset+numVars} inclusive
   */
  
  def balanced(numVars: Int, offset: Int=0): VTree[_] = balancedImpl(numVars,offset)
  
  private def balancedImpl(numVars: Int, offset: Int): VTreeImpl = {
    assume(numVars > 0)
    assume(offset >=0)
    if(numVars == 1) return new VTreeLeafImpl(offset+1)
    else return new VTreeINodeImpl(balancedImpl(numVars/2,offset),balancedImpl(numVars-numVars/2,offset+numVars/2))
  }
  
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
  
  def variables = vl.variables union vr.variables
  
  def children = Seq(vl,vr)
  
}

//************************
// vtree implementations
//************************

abstract class VTreeImpl extends VTree[VTreeImpl]{
  
  private var _parent: Option[VTreeINodeImpl] = None
  protected[logic] def setParent(p: VTreeINodeImpl){
    require(p != null)
    require(_parent.isEmpty, s"DoubleLinkedTree $this cannot have multiple parents ($parent and $p)")
    _parent = Some(p)
  }
  
  def parent: Option[VTreeINodeImpl] = _parent
  
  override val root = super.root
  
}

class VTreeLeafImpl(val variable: Variable) extends VTreeImpl with VTreeLeaf[VTreeImpl] {
    
  override val variables = super.variables
  
}

class VTreeINodeImpl(val vl: VTreeImpl, val vr: VTreeImpl) extends VTreeImpl with VTreeINode[VTreeImpl] {

  vl.setParent(this)
  vr.setParent(this)
  
  override val variables = super.variables
  override val children = super.children
  
}