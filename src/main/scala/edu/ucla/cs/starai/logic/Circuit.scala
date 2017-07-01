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

trait Tractable {
    
  self: Circuit[_] =>
    
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

/**
 * A circuit that can be modified and composed
 * Is invariant in N because of apply arguments being N
 */
trait ComposableCircuit[N <: ComposableCircuit[N]] extends Circuit[N] {
  
  self: ComposableCircuit[N] with N =>
    
  def unary_!(): N
  
  /**
   * Logical conditioning (the result no longer mentions l)
   */
  def |(l: Literal): N
  
  /**
   * Logical assignment (the result mentions l and assigns it to true)
   */
  def assign(l: Literal): N
  
  // Any output will always respect the LCA of the operand vtrees
  def &&(other: N): N
  def ||(other: N): N
  
  // derived operators
  def forget(v: Variable) = ((this | v) || (this | !v))
  final def unary_¬() = unary_!()
  final def ∧(other: N) = &&(other)
  final def ∨(other: N) = ||(other)
  
  //TODO add specialized "conjoin with clause" to speed up CNF compilation
  
}