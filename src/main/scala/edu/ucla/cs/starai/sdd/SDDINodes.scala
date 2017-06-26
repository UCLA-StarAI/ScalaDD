package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.Variable
import edu.ucla.cs.starai.util.BigRational
import edu.ucla.cs.starai.logic.VTree

trait SDDINode extends SDD{
  
  override def vtree: VTreeINode[_]
  
  def name = s"N$hashCode"
  
}

trait ElementNode extends SDDINode {
  
  assume(prime.subRespects(vtree.vl), "XY-Partitions should respect the vtree: " + prime)
  assume(sub.subRespects(vtree.vr), "XY-Partitions should respect the vtree: " + sub)
  
  def prime: SDD
  def sub: SDD

  def vl = vtree.vl
  def vr = vtree.vr
  
  def children = Seq(prime,sub)
  
  def usedVars(cache: Cache[Set[Variable]]) = prime.usedVars(cache) union sub.usedVars(cache)
  
  def isConsistent(cache: Cache[Boolean]) = prime.isConsistent(cache) && sub.isConsistent(cache)
  def isValid = prime.isValid && sub.isValid
  
  def modelRatio(cache: Cache[BigRational]) = prime.modelRatio(cache) * sub.modelRatio(cache)
      
  override def toString = s"[$prime,$sub]"

}
  

trait DecisionNode extends SDDINode {
  
  assume(elems.forall { _.vtree == elems.head.vtree }, "XY-Partitions should have elements respecting the same vtree: " + elems)
  assume(primes.forall{_.subRespects(vtree.vl)},"decompositions follow the vtree")
  assume(subs.forall{_.subRespects(vtree.vr)},"decompositions follow the vtree")
  
  def elems: Seq[ElementNode]
  def partitionSize = elems.size
  
  def primes = elems.map(_.prime)
  def subs = elems.map(_.sub)
  
  def children = elems
  
  override def usedVars(cache: Cache[Set[Variable]]) = cache.getOrElseUpdate(this, {
    val usedDecendents = if(isPrimeTrimmable) subs else if(isSubTrimmable) primes else elems
    usedDecendents.map(_.usedVars(cache)).reduce(_ union _)
  })
  
  def isConsistent(cache: Cache[Boolean]) = elems.exists(_.isConsistent(cache))
  def isValid = (BigRational(1) == (modelRatio: BigRational))
    
  def modelRatio(cache: Cache[BigRational]) = cache.getOrElse(this,{
    elems.map(_.modelRatio(cache)).sum
  })
  
  /**
   * Define what types of trimming are possible on this node        
   */
  def isSubTrimmableFirst = (partitionSize == 2 && elems(0).sub.isValid && !elems(1).sub.isConsistent)
  def isSubTrimmableSecond = (partitionSize == 2 && elems(1).sub.isValid && !elems(0).sub.isConsistent)
  def isSubTrimmable = (isSubTrimmableFirst || isSubTrimmableSecond)
  def isPrimeTrimmable = (partitionSize == 1 && elems(0).prime.isValid)
  def isTrimmable = isPrimeTrimmable || isSubTrimmable
  
}

