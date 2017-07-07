/*
 * Copyright 2017 Guy Van den Broeck <guyvdb@cs.ucla.edu>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    (modelRatio * (BigInt(2).pow(numVariables))).toExactBigInt
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
  // one of these needs to be overridden to implement both
  def &&(that: N): N = !(!this || !that)
  def ||(that: N): N = !(!this && !that)
  
  // derived operators
  def forget(v: Variable) = ((this | v) || (this | !v))
  final def unary_¬() = unary_!()
  final def ∧(that: N) = &&(that)
  final def ∨(that: N) = ||(that)
  
  //TODO add specialized "conjoin with clause" to speed up CNF compilation
  
}