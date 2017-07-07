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

package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.logic
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util._
import scala.language.existentials
import scala.math.BigInt.int2bigInt
import scala.util.hashing.MurmurHash3


trait SDD extends Circuit[SDD] with Tractable {
    
  def vtree: VTree.Some // silly that these bounds need to be repeated
    
  def respects(vtree: VTree[_]) = (vtree == this.vtree)
  def subRespects(vtree: VTree[_]) = (vtree.contains(this.vtree))
  
  def variables: Set[Variable] = vtree.variables
  def numVariables = vtree.numVariables
    
  def decisions = collect{case dec:DecisionNode[_] => dec}
  def terminals = collect{case leaf:TerminalNode => leaf}
  
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
    
  def kind: Either[TerminalNode,DecisionNode[_]]
  
  def name: String
  override def toString = name
  
}

object SDD {
  
}

/**
 * A terminal SDD node could respect a non-terminal vtree node.
 */
trait TerminalNode extends SDD{
    
  override def children = Seq()
  
  def kind = Left(this)
    
  def terminalKind: Either3[LiteralNode,TrueNode,FalseNode]

}

/**
 * Constant or literal nodes are not necessarily leaves (e.g., in normalized SDDs)
 */
trait TrueNode extends SDD{
  
  def terminalKind = Mid3(this)
  
  // needs to use override everywhere to allow mixing of traits
  override def usedVars(cache: Cache[Set[Variable]]) = Set.empty
  
  override def modelRatio(cache: Cache[BigRational]) = 1
  
  final override def isConsistent = true
  final override def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = true
  
  override def name = s"true[${vtree.variables.mkString(",")}]"
}


trait FalseNode extends SDD{
  
  def terminalKind = Right3(this)
  
  override def usedVars(cache: Cache[Set[Variable]]) = Set.empty
  
  override def modelRatio(cache: Cache[BigRational]) = 0
  
  final override def isConsistent = false
  final override def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = false
  
  override def name = s"false[${vtree.variables.mkString(",")}]"
  
}


trait LiteralNode extends SDD {
    
  def terminalKind = Left3(this)
  
  override def modelRatio(cache: Cache[BigRational]) = BigRational(1,2)
  
  assume(variables.contains(variable), s"Leafs should respect the vtree: ${variables} contains ${variable}")
  
  def literal: logic.Literal
  def variable = literal.variable
  
  override def usedVars(cache: Cache[Set[Variable]]) = Set(variable)
  
  final override def isConsistent = true
  final override def isConsistent(cache: Cache[Boolean]) = isConsistent
  
  final override def isValid = false
  
  override def name = literal.toString
  
}

trait DecisionNode[+N <: SDD] extends SDD {
  
  self: N =>
  
  override def vtree: VTreeINode[T] forSome { type T <: VTree[T] }
  
  def decomp: XYDecomposition[N]
  
  assumes(decomp.elements, (_:Element[N]).subRespects(vtree))
  
  def kind = Right(this)
  
  def name = s"N$hashCode"
  
  def primes: Iterable[N] = decomp.primes
  def subs: Iterable[N] = decomp.subs
  def children: Seq[N] = decomp.children
  def partitionSize = decomp.size
  
  override def usedVars(cache: Cache[Set[Variable]]) = cache.getOrElseUpdate(this, {
    val usedDecendents = if(isPrimeTrimmable) subs else if(isSubTrimmable) primes else children
    usedDecendents.map(_.usedVars(cache)).reduce(_ union _)
  })
  
  def isConsistent(cache: Cache[Boolean]) = decomp.isConsistent(cache)
  
  //TODO provide specialized implementation
  def isValid = (BigRational(1) == (modelRatio: BigRational))
    
  def modelRatio(cache: Cache[BigRational]) = cache.getOrElse(this,decomp.modelRatio(cache))
  
  /**
   * Define what types of trimming are possible on this node        
   */
  def isSubTrimmable = decomp.isSubTrimmable
  def isPrimeTrimmable = decomp.isPrimeTrimmable
  def isTrimmable = decomp.isTrimmable
  
}