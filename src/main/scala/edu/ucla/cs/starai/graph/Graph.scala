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

trait Graph[+N] extends Iterable[N] {
      
  /**
   * Does this graph contain the given subgraph?
   * Takes any graph in order to covariant in N
   */
  def contains[M >: N](node: M): Boolean = exists { node == _ }
  
  def numNodes: Int = iterator.length
  def numEdges: Int
  
  def linearize: Seq[N] = {
    var nodes: List[N] = Nil
    for(node <- this) nodes = node :: nodes
    nodes.reverse
  }
  
  override def iterator: Iterator[N] = linearize.iterator
  
}