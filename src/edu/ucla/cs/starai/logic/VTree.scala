package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.TreeINode
import edu.ucla.cs.starai.graph.TreeLeaf

sealed abstract class VTree extends Tree[VTree,VTreeLeaf,VTreeINode] {

  def variables: Set[Variable]
  def numVariables = variables.size

}

object VTree{
  
  /**
   * Generate balanced vtree for variables X{offset+1} to X{offset+numVars} inclusive
   */
  def balanced(numVars: Int, offset: Int=0): VTree = {
    assume(numVars > 0)
    assume(offset >=0)
    if(numVars == 1) return new VTreeLeaf(offset+1)
    else return new VTreeINode(balanced(numVars/2,offset),balanced(numVars-numVars/2,offset+numVars/2))
  }
  
}


class VTreeLeaf(val variable: Variable) extends VTree with TreeLeaf[VTree,VTreeLeaf,VTreeINode] {

  override val variables = Set(variable)
  
}

class VTreeINode(val vl: VTree, val vr: VTree) extends VTree with TreeINode[VTree,VTreeLeaf,VTreeINode] {

  assume((vl.variables intersect vr.variables).isEmpty, "Variables in left and right branch should be disjoint.")

  override val variables = vl.variables union vr.variables
  
  override val children = Seq(vl,vr)
  
}