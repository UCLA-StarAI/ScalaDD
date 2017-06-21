package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.logic.Literal
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf

trait SDD extends DAG[SDD,SDDLeaf, SDDINode] {
  
  def vtree: VTree
    
  def respects(vtree: VTree) = (vtree == this.vtree)
  
  def isConsistent: Boolean
  def isValid: Boolean
  
  def decNodes = inodes.collect{case decNode:SDDDecNode => decNode}
  
  def sddSize: Long = decNodes.map(_.partitionSize).sum
  def sddNodes: Long = decNodes.size
  
  // TODO move to pimp my library pattern
  def modelCount: BigInt = {
    def input(leaf: SDDLeaf) = leaf match{
          case _:FalseLeaf => 0
          case _:LiteralLeaf => 1
          case _:TrueLeaf => 2
    }
    def propagate(inode: SDDINode, values: Seq[BigInt]) = inode match{
          case _:SDDElemNode => values.product
          case _:SDDDecNode => values.sum
    }
    foldUp[BigInt](input, propagate)
  }
  
  def name: String
  override def toString = name
  
}


trait SDDLeaf extends SDD with LeafNode[SDD,SDDLeaf,SDDINode]{
  
  def vtree: VTreeLeaf
  
}


trait SDDINode extends SDD with INode[SDD,SDDLeaf,SDDINode]{
  
  def vtree: VTreeINode
  def name = s"N$hashCode"
  
}


// Concrete classes

trait TrueLeaf extends SDDLeaf {
  def isConsistent = true
  def isValid = true
  override def name = s"true (${vtree.variable})"
}


trait FalseLeaf extends SDDLeaf {
  def isConsistent = false
  def isValid = false
  override def name = "false"
}


trait LiteralLeaf extends SDDLeaf {
  
  assume(vtree.variable == literal.variable, s"Leafs should respect the vtree: ${vtree.variable} == ${literal.variable}")
  
  def literal: Literal
  final def isConsistent = true
  final def isValid = false
  override def name = literal.toString
  
}


trait SDDElemNode extends SDDINode {
  
  assume(prime.respects(vtree.vl), "XY-Partitions should respect the vtree: " + prime)
  assume(sub.respects(vtree.vr), "XY-Partitions should respect the vtree: " + sub)
  
  def prime: Prime
  def sub: Sub
  
  def children = Seq(prime,sub)
  
  override def toString = s"[$prime,$sub]"

}
  

trait SDDDecNode 
    extends SDDINode {

  assume(elems.forall { _.vtree == elems.head.vtree }, "XY-Partitions should have elements respecting the same vtree: " + elems)
  
  def elems: Seq[SDDElemNode]
  def partitionSize = elems.size
  
  def children = elems.toSeq
  
  
}

