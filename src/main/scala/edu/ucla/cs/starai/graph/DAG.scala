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

package edu.ucla.cs.starai.graph

import scala.collection.immutable
import scala.collection.mutable

import edu.ucla.cs.starai.util._

/**
 * A directed acyclic graph with nodes N
 * Generic types for inner nodes and leaf nodes are omitted to avoid clutter down the hierarchy
 */
trait DAG[+N <: DAG[N]] extends Graph[N] with SelfTraversable[N] with Caching[DAG[_]]{
  
   //self type enforces that object's subtype of DAG is also the subtype of DAG passed as N
   self: N =>
     
  def asSelf: DAG[N] with N = this
    
  def foldUp[T](
    propagate: (N,Seq[T]) => T): T  = {
    foldUpCached(propagate,emptyCache)
  }
    
  def foldUpCached[T](
    propagate: (N,Seq[T]) => T,
    cache: Cache[T]): T = {
    cache.getOrElseUpdate(this, 
        propagate(this,children.map(_.foldUpCached(propagate,cache))))
  }
  
  def children: Seq[N]
  
  def numChildren: Int = children.size
  
  override def foreach[U](f: N => U): Unit = {
    foldUp((x,_:Any) => f(x))
  }
    
  def numEdges: Int = {
    var count = 0;
    foldUp[Unit]{(n,c) => count = count + c.length}
    count
  }
  
  /**
   * Does this graph contain the given subgraph?
   * Takes any graph in order to covariant in N
   */
  def containsNode[M >: N <: DAG[_]](node: M): Boolean = exists { node == _ }
  
}