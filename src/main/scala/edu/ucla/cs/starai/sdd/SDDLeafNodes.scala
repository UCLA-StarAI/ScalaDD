package edu.ucla.cs.starai.sdd

import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.logic
import edu.ucla.cs.starai.logic.Variable
import edu.ucla.cs.starai.util.BigRational
import edu.ucla.cs.starai.logic.VTree

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
