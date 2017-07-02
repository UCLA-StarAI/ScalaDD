package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.sdd.SDD

trait VTreeImpl extends VTree[VTreeImpl] with Child[VTreeImpl]

object VTreeImpl{
  
  def balanced(numVars: Int, offset: Int=0): VTreeImpl = {
    assume(numVars > 0)
    assume(offset >=0)
    if(numVars == 1) return new VTreeLeafImpl(offset+1)
    else return new VTreeINodeImpl(
        balanced(numVars/2,offset),
        balanced(numVars-numVars/2,offset+numVars/2))
  }
  
}

class VTreeLeafImpl(val variable: Variable) extends VTreeImpl with VTreeLeaf[VTreeImpl] {
    
  override val variables = super.variables
  
}

class VTreeINodeImpl(val vl: VTreeImpl, val vr: VTreeImpl) 
  extends VTreeImpl with VTreeINode[VTreeImpl] {

  vl.setParent(this)
  vr.setParent(this)
  
}