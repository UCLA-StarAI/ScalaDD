package edu.ucla.cs.starai.sdd.manager

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.SDDINode
import edu.ucla.cs.starai.sdd.SDDLeaf
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.util.ProxyKey




/**
 * Extends SDD with operations that require an SDD manager
 **/
trait ManagedSDD 
  extends SDD 
  with DAG[ManagedSDD,ManagedSDDLeaf, ManagedSDDINode]  {
    
  def manager: SDDManager
  def vtree = manager.vtree
  
  def unary_! : ManagedSDD
  def |(l: Literal): ManagedSDD
  def &&(other: ManagedSDD): ManagedSDD
  def ||(other: ManagedSDD): ManagedSDD
  
  override def decNodes: Iterator[ManagedSDDDecNode] = inodes.collect{case decNode:ManagedSDDDecNode => decNode}
  
  private def selectNonTrimmed(node:ManagedSDD) = node match{
      case decNode: ManagedSDDDecNode => !decNode.isTrimmable
      case _ => true
    }
  
  def trimmedSddSize: Long = {
    var count = 0L;
    def f(inode: ManagedSDD) = inode match{
      case decNode: ManagedSDDDecNode => count = count + decNode.partitionSize
      case _ => {}
    }
    selectiveForeach(selectNonTrimmed, f)
    count
  }
  
  
  def trimmedSddNodes: Long = {
    var count = 0L;
    def f(inode: ManagedSDD) = inode match{
      case decNode: ManagedSDDDecNode => count = count + 1
      case _ => {}
    }
    selectiveForeach(selectNonTrimmed, f)
    count
  }
  
  def unUsedVars = vtree.variables -- usedVars
  
  def usedVars: Set[Variable] = {
    def input(leaf: ManagedSDDLeaf) = leaf match{
          case _:ManagedLiteralLeaf => Set(leaf.variable)
          case _ => Set.empty[Variable]
    }
    def propagate(inode: ManagedSDDINode, values: Seq[Set[Variable]]) = {
      val usedVarsBelow = values.reduce(_ union _)
      inode match{
          case _:ManagedSDDElemNode => usedVarsBelow
          case inode:ManagedSDDDecNode => {
            if(inode.isPrimeTrimmable) usedVarsBelow -- inode.vtree.vl.variables
            else if(inode.isSubTrimmable) usedVarsBelow -- inode.vtree.vr.variables
            else usedVarsBelow
          }
      }
    }
    foldUp[Set[Variable]](input, propagate)
  }
  
  def usedVarsModelCount: BigInt = modelCount >> (unUsedVars.size)
  
}

trait ManagedSDDWithKey
  extends ManagedSDD 
  with ProxyKey[Long]{
    
  def unary_! : ManagedSDDWithKey
  def |(l: Literal): ManagedSDDWithKey
  def &&(other: ManagedSDD): ManagedSDDWithKey
  def ||(other: ManagedSDD): ManagedSDDWithKey
  
}


trait ManagedSDDLeaf 
  extends ManagedSDD 
  with SDDLeaf 
  with LeafNode[ManagedSDD,ManagedSDDLeaf, ManagedSDDINode]
  with ManagedSDDWithKey{
  
  def manager: SDDManagerLeaf
  override def vtree: VTreeLeaf = manager.vtree
  
  override def unary_! : ManagedSDDLeaf
    
}


trait ManagedSDDINode 
  extends ManagedSDD 
  with SDDINode 
  with INode[ManagedSDD,ManagedSDDLeaf, ManagedSDDINode]{
  
  def manager: SDDManagerINode
  override def vtree: VTreeINode = manager.vtree
  
  override def unary_! : ManagedSDDINode
  
}
