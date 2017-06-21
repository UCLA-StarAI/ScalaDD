package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.logic.Literal
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.sdd.manager.ManagedSDDWithKey
import edu.ucla.cs.starai.sdd.manager.SDDManager
import edu.ucla.cs.starai.sdd.manager.SDDManagerINode
import edu.ucla.cs.starai.sdd.manager.SDDManagerLeaf


/**
 * Extends SDD with operations that require an SDD manager
 **/
trait ManagedSDD 
  extends DAG[ManagedSDD,ManagedSDDLeaf,ManagedSDDINode]   
  with SDD[ManagedSDD] 
  with Rewireable[ManagedSDD] {
    
  def manager: SDDManager
  def vtree = manager.vtree
  
  def unary_! : ManagedSDD
  def |(l: Literal): ManagedSDD
  def &&(other: ManagedSDD): ManagedSDD
  def ||(other: ManagedSDD): ManagedSDD
  
  override def decNodes: Iterator[ManagedSDDDecNode] = inodes.collect{case decNode:ManagedSDDDecNode => decNode}
  
  def trim : ManagedSDD = {
    def input(leaf: ManagedSDDLeaf) = leaf
    def propagate(inode: ManagedSDDINode, trimmedChildren: Seq[ManagedSDD]) = {
      inode match{
          case inode:ManagedSDDElemNode => inode.rewire(trimmedChildren(0),trimmedChildren(1))
          case inode:ManagedSDDDecNode => {
            if(inode.isPrimeTrimmable) trimmedChildren(0).asInstanceOf[ManagedSDDElemNode].sub
            else if(inode.isSubTrimmableFirst) trimmedChildren(0).asInstanceOf[ManagedSDDElemNode].prime
            else if(inode.isSubTrimmableSecond) trimmedChildren(1).asInstanceOf[ManagedSDDElemNode].prime
            else inode.rewire(trimmedChildren)
          }
      }
    }
    foldUp[ManagedSDD](input, propagate)
  }
  
  def trimmedSize = trim.sddSize
  def trimmedNumNodes = trim.sddNodes
  
}

trait ManagedSDDLeaf 
  extends LeafNode[ManagedSDD,ManagedSDDLeaf, ManagedSDDINode] 
  with ManagedSDD 
  with SDDLeaf[ManagedSDD] 
  with ManagedSDDWithKey{
  
  def manager: SDDManagerLeaf
  override def vtree: VTreeLeaf = manager.vtree
  
  override def unary_! : ManagedSDDLeaf
      
}

trait ManagedSDDINode 
  extends INode[ManagedSDD,ManagedSDDLeaf,ManagedSDDINode]
  with ManagedSDD 
  with SDDINode[ManagedSDD] {
  
  def manager: SDDManagerINode
  override def vtree: VTreeINode = manager.vtree
  
  override def unary_! : ManagedSDDINode
  
}

trait ManagedSDDDecNode 
  extends ManagedSDDINode 
  with RewireableDecNode[ManagedSDD,ManagedSDDDecNode]{
    
}

trait ManagedSDDElemNode 
  extends ManagedSDDINode 
  with RewireableElemNode[ManagedSDD,ManagedSDDElemNode] {
    
}


