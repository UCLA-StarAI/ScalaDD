package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt
import scala.collection.mutable

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util._


trait SDD extends TractableCircuit[SDD]{
  
  def vtree: VTree[VTree[_]]
    
  def respects(vtree: VTree[_]) = (vtree == this.vtree)
  def subRespects(vtree: VTree[_]) = (vtree.contains(this.vtree))
  
  def variables = vtree.variables
  def numVariables = vtree.numVariables
    
  def decisions = collect{case decNode:DecisionNode => decNode}
  def elements = collect{case elemNode:ElementNode => elemNode}
  
  def numElements: Long = decisions.map(_.partitionSize).sum
  def sddSize = numElements
  def numDecisions: Long = decisions.size
        
  def usedVars: Set[Variable] = usedVars(emptyCache)
  def usedVars(cache: Cache[Set[Variable]]): Set[Variable] 
  def unUsedVars = vtree.variables -- usedVars
      
  def isConsistent = isConsistent(emptyCache)
  def isConsistent(cache: Cache[Boolean]): Boolean
  
  def isValid: Boolean
  
  def modelRatio = modelRatio(emptyCache)
  def modelRatio(cache: Cache[BigRational]): BigRational
  
  def usedVarsModelCount: BigInt = modelCount >> (unUsedVars.size)
    
  def name: String
  override def toString = name
  
}

object SDD {
  
}
