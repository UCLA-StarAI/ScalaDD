/*
 * Copyright 2017 Guy Van den Broeck
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

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree

/**
 * A builder of SDDs (encapsulating the right vtree to use).
 */
trait SDDBuilder[N <: SDD]{
    
  /**
   * Builds a representation of the given SDD according to the conventions of this manager.
   * Use case: take an SDD for a sub-vtree and lift it to the vtree of this manager.
   */
  def normalize(sdd: N): N
  
  /**
   * Consistent, mutually exclusive and exhaustive primes with their subs
   */
  def partition(primes: Seq[N],subs: Seq[N]): N
  
  /**
   * Decomposable conjunction. 
   * Arguments are in any order but are assumed to respect a lower vtree.
   */
  def indepConjoin(x: N, y: N): N
  
  /**
   * Requires that l.variable is in vtree
   */
  def literal(l: Literal): N
  def False(): N
  def True(): N
  
}

trait BuilderVTree[N <: SDD] extends DoubleLinkedTree[BuilderVTree[N]] 
  with VTree[BuilderVTree[N]] 
  with SDDBuilder[N] {
  
  
}