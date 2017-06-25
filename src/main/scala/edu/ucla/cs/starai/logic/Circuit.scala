package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.util.BigRational

/**
 * A logical circuit
 */
trait Circuit[+N <: Circuit[N]] extends DAG[N] {
  
  self: N =>
  
  def variables: Set[Variable]
  def numVariables: Int
  
}

trait TractableCircuit[+N <: TractableCircuit[N]] extends Circuit[N]{
    
  self: N =>
    
  def isConsistent: Boolean
  def isValid: Boolean  
  
  /*
   * Fraction of assignments that are models
   */
  def modelRatio: BigRational
  
  def modelCount: BigInt = {
    (modelRatio * (BigInt(2)^(numVariables))).toExactBigInt
  }
  
}
