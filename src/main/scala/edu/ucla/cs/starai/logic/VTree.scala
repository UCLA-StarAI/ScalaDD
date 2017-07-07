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

import scala.language.existentials
import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.sdd.SDD

trait VTree[+N <: VTree[N]] extends DoubleLinkedTree[N] {
  
  self: N =>
 
  def kind: Either[VTreeLeaf[N],VTreeINode[N]]
  
  def contains(v: Variable): Boolean = variables.contains(v)
  def variables: Set[Variable]
  def literals: Set[Literal] = variables.map(!_.toLiteral) union variables.map(_.toLiteral)
  def numVariables = variables.size
    
  def lca(v: Variable): N = {
    if(variables.contains(v)) this
    else ancestors.find(_.contains(v)).get
  }
  
  def childFor(v: Variable): Option[N]
  def splitFor(v1: Variable, v2: Variable): Option[(N,N,VTreeINode[N] with N)]
  
  override def toString = s"VTree over ${variables.mkString(", ")}"
  
}

object VTree{
  
  type Some = VTree[T] forSome { type T <: VTree[T] }
  
  /**
   * Generate balanced vtree for variables X{offset+1} to X{offset+numVars} inclusive
   */
  def balanced(numVars: Int, offset: Int=0): Some = 
    VTreeImpl.balanced(numVars,offset)
  
}

trait VTreeLeaf[+N <: VTree[N]] extends VTree[N] {
  
  self: N =>
  
  override def children = Seq()
  
  def variable: Variable
  
  def kind = Left(this)
  
  override def contains(v: Variable) = (v == variable)
  def variables: Set[Variable] = Set(variable)
  override def numVariables = 1
  
  override def contains[U >: N](that: U) = (that == this)
  
  def childFor(v: Variable) = None
  def splitFor(v1: Variable, v2: Variable) = None
  
}

trait VTreeINode[+N <: VTree[N]] extends VTree[N] {
  self: N =>

  assume(!(vl.variables overlaps vr.variables), "Variables in left and right branch should be disjoint.")
    
  def vl: VTree[N] with N 
  def vr: VTree[N] with N
  
  def kind = Right(this)
  
  override val variables = vl.variables union vr.variables
  override val children = Seq(vl,vr)
  
  def splitFor(v1: Variable, v2: Variable): Option[(N,N,VTreeINode[N] with N)] = {
    if(vl.contains(v1) && vr.contains(v2)) Some((vl,vr,this))
    else if(vl.contains(v2) && vr.contains(v1)) Some((vr,vl,this))
    else if(vl.contains(v1) && vl.contains(v2)) vl.splitFor(v1,v2)
    else if(vr.contains(v1) && vr.contains(v2)) vr.splitFor(v1,v2)
    else None
  }
  
  def childFor(v: Variable) = {
    if(vl.contains(v)) Some(vl)
    else if(vr.contains(v)) Some(vr)
    else None
  }
    
  private[this] val containsCache: Set[Any] = iterator.toSet
  override def contains[U >: N](that: U) = containsCache.contains(that)
  
}



object VTreeINode{
  
  type Some = VTreeINode[T] forSome { type T <: VTree[T] }
  
}