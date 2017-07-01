package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.sdd.SDD


class VTreeLeafImpl(val variable: Variable) extends ChildVTree with VTreeLeaf[ChildVTree] {
    
  override val variables = super.variables
  
}

class VTreeINodeImpl(val vl: ChildVTree, val vr: ChildVTree) 
  extends ChildVTree with VTreeINode[ChildVTree] {

  vl.setParent(this)
  vr.setParent(this)
  
  override val variables = super.variables
  override val children = super.children
  
}