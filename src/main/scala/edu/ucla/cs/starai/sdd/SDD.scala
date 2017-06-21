package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.util._

trait SDD[+N <: SDD[N]] extends DAG[SDD[N],SDDLeaf[N],SDDINode[N]] {
  self: N =>
  
  def vtree: VTree
    
  def respects(vtree: VTree) = (vtree == this.vtree)
  def subRespects(vtree: VTree) = (vtree.contains(this.vtree))
  
  def isConsistent: Boolean
  def isValid: Boolean
  
  def decNodes = inodes.collect{case decNode:SDDDecNode[N] => decNode}
  
  def sddSize: Long = decNodes.map(_.partitionSize).sum
  def sddNodes: Long = decNodes.size
  
  // not sure why we need this... why doesn't the type system enforce the self type?
  def asSelfSDD: N = this
  
  // TODO move to pimp my library pattern
  def modelCount: BigInt = {
    def input(leaf: SDDLeaf[N]) = leaf match{
          case _:FalseLeaf[N] => 0
          case _:LiteralLeaf[N] => 1
          case _:TrueLeaf[N] => 2
    }
    def propagate(inode: SDDINode[N], values: Seq[BigInt]) = inode match{
          case _:SDDElemNode[N] => values.product
          case _:SDDDecNode[N] => values.sum
    }
    foldUp[BigInt](input, propagate)
  }
    
  def unUsedVars = vtree.variables -- usedVars
  
  def usedVars: Set[Variable] = {
    def input(leaf: SDDLeaf[N]) = leaf match{
          case _:LiteralLeaf[N] => Set(leaf.variable)
          case _ => Set.empty[Variable]
    }
    def propagate(inode: SDDINode[N], values: Seq[Set[Variable]]) = {
      val usedVarsBelow = values.reduce(_ union _)
      inode match{
          case _:SDDElemNode[N] => usedVarsBelow
          case inode:SDDDecNode[N] => {
            if(inode.isPrimeTrimmable) usedVarsBelow -- inode.vtree.vl.variables
            else if(inode.isSubTrimmable) usedVarsBelow -- inode.vtree.vr.variables
            else usedVarsBelow
          }
      }
    }
    foldUp[Set[Variable]](input, propagate)
  }
  
  def usedVarsModelCount: BigInt = modelCount >> (unUsedVars.size)
    
  def name: String
  override def toString = name
  
}

trait SDDRoot[+N <: SDD[N]] extends SDD[N]{
  self : N =>
  
}

trait SDDLeaf[+N <: SDD[N]] extends SDDRoot[N] with LeafNode[SDD[N],SDDLeaf[N],SDDINode[N]]{
  self : N =>
  
  def vtree: VTreeLeaf
  final def variable = vtree.variable
}


trait SDDINode[+N <: SDD[N]] extends SDD[N] with INode[SDD[N],SDDLeaf[N],SDDINode[N]]{
  self : N =>
  
  def vtree: VTreeINode
  def name = s"N$hashCode"
  
}


// Concrete classes

trait TrueLeaf[+N <: SDD[N]] extends SDDLeaf[N] {
  self : N =>
  
  def isConsistent = true
  def isValid = true
  override def name = s"true (${vtree.variable})"
}


trait FalseLeaf[+N <: SDD[N]] extends SDDLeaf[N]{
  self : N =>
  
  def isConsistent = false
  def isValid = false
  override def name = "false"
}


trait LiteralLeaf[+N <: SDD[N]] extends SDDLeaf[N] {
  self : N =>
  
  
  assume(variable == literal.variable, s"Leafs should respect the vtree: ${variable} == ${literal.variable}")
  
  def literal: Literal
  final def isConsistent = true
  final def isValid = false
  override def name = literal.toString
  
}


trait SDDElemNode[+N <: SDD[N]] extends SDDINode[N] {
  self : N =>
  
  assume(prime.subRespects(vtree.vl), "XY-Partitions should respect the vtree: " + prime)
  assume(sub.subRespects(vtree.vr), "XY-Partitions should respect the vtree: " + sub)
  
  def prime: SDD[N] with N
  def sub: SDD[N] with N
  
  def children = Seq(prime,sub)
  
  override def toString = s"[$prime,$sub]"

}
  

trait SDDDecNode[+N <: SDD[N]]
    extends SDDINode[N] with SDDRoot[N] {
  self : N =>
  
  assume(elems.forall { _.vtree == elems.head.vtree }, "XY-Partitions should have elements respecting the same vtree: " + elems)
  
  def elems: Seq[SDDElemNode[N] with N]
  def partitionSize = elems.size
  
  def primes = elems.map(_.prime)
  def subs = elems.map(_.sub)
  
  def children = elems.toSeq
  
  /**
   * Define what types of trimming are possible on this node        
   */
  def isSubTrimmableFirst = (partitionSize == 2 && elems(0).sub.isValid && !elems(1).sub.isConsistent)
  def isSubTrimmableSecond = (partitionSize == 2 && elems(1).sub.isValid && !elems(0).sub.isConsistent)
  def isSubTrimmable = (isSubTrimmableFirst || isSubTrimmableSecond)
  def isPrimeTrimmable = (partitionSize == 1 && elems(0).prime.isValid)
  def isTrimmable = isPrimeTrimmable || isSubTrimmable
  
}

