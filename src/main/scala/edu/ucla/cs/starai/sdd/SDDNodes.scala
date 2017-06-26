package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.logic
import edu.ucla.cs.starai.logic.Variable
import edu.ucla.cs.starai.util.BigRational
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode

trait SDDLeaf extends SDD{
    
  override def children = Seq()
    
}

trait TrueNode extends SDDLeaf {
  
  def usedVars(cache: Cache[Set[Variable]]) = Set.empty
  
  def modelRatio(cache: Cache[BigRational]) = 1
  
  final override def isConsistent = true
  final def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = true
  
  def name = s"true (${vtree.variables})"
}


trait FalseNode extends SDDLeaf{
  
  def usedVars(cache: Cache[Set[Variable]]) = Set.empty
  
  def modelRatio(cache: Cache[BigRational]) = 0
  
  final override def isConsistent = false
  final def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = false
  
  def name = "false"
}


trait LiteralNode extends SDDLeaf {
    
  def modelRatio(cache: Cache[BigRational]) = BigRational(1,2)
  
  assume(variables.contains(variable), s"Leafs should respect the vtree: ${variables} contains ${variable}")
  
  def literal: logic.Literal
  def variable = literal.variable
  
  def usedVars(cache: Cache[Set[Variable]]) = Set(variable)
  
  final override def isConsistent = true
  final def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = false
  
  def name = literal.toString
  
}

trait DecisionNode[+N <: SDD] extends SDD {
  
  self: N =>
  
  override def vtree: VTreeINode[_]
  
  assume(primes.forall{_.subRespects(vtree.vl)},"decompositions follow the vtree")
  assume(subs.forall{_.subRespects(vtree.vr)},"decompositions follow the vtree")
  
  def name = s"N$hashCode"
  
  def elems: Seq[Element]
  def primes: Seq[N] = elems.map(_.prime)
  def subs: Seq[N] = elems.map(_.sub)
  def children: Seq[N] = (primes interleave subs)
  
  def partitionSize = elems.size
  
  override def usedVars(cache: Cache[Set[Variable]]) = cache.getOrElseUpdate(this, {
    val usedDecendents = if(isPrimeTrimmable) subs else if(isSubTrimmable) primes else children
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
  
  trait Element {
    
    assume(prime.subRespects(vtree.vl), "XY-Partitions should respect the vtree: " + prime)
    assume(sub.subRespects(vtree.vr), "XY-Partitions should respect the vtree: " + sub)
    
    def prime: N
    def sub: N
            
    def isConsistent(cache: Cache[Boolean]) = prime.isConsistent(cache) && sub.isConsistent(cache)
    def modelRatio(cache: Cache[BigRational]) = prime.modelRatio(cache) * sub.modelRatio(cache)
  
    override def toString = s"[$prime,$sub]"
  
  }
  
}



