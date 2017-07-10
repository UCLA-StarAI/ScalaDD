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

import edu.ucla.cs.starai.logic._
import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheStats
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.graph.DAG
import scala.language.existentials

/**
 * These are not implemented here because they are likely to require specialized implementations anyway
 */
trait ComposableXYDecomposition[N <: ComposableSDD[N]] extends XYDecomposition[N]{

  // do not refer back to SDD nodes or manager that could refer to this object
  
  //TODO check whether unique nodes cache is broken by reference to vtree here (and parent references in vtree).
  
  //TODO consider sorting elements and associating a designated unique nodes and apply cache with the first prime!
  
  def elements: Seq[ComposableElement[N]]
    
  def mapPrimes(f: N => N): ComposableXYDecomposition[N] 
  
  def mapPrimes(f: N => N, extraPrime:N, extraSub: N): ComposableXYDecomposition[N] 
  
  def mapSubs(f: N => N): ComposableXYDecomposition[N] 
  
  def &&(that: ComposableXYDecomposition[N]): ComposableXYDecomposition[N]
  
  // specialize instead of using mapSubs because negation cannot undo compression
  def unary_!(): ComposableXYDecomposition[N]
  
}

sealed trait ComposableElement[N <: ComposableSDD[N]] extends Element[N] {
  
  def &&(that: ComposableElement[N]): Option[ComposableElement[N]] = {
    val newPrime = that.prime && this.prime
    if(newPrime.isConsistent) 
      Some(new ComposableElementImpl(newPrime, ComposableElement.this.sub && that.sub))
    else None
  }
  
  def mapPrime(f: N => N): Option[ComposableElement[N]] = {
    val fprime = f(prime)
    if(fprime.isConsistent) Some(new ComposableElementImpl(fprime, sub))
    else None
  }
  
  def mapSub(f: N => N): ComposableElement[N] = {
    new ComposableElementImpl(prime,f(sub))
  }

}

final class ComposableElementImpl[N <: ComposableSDD[N]](val prime: N, val sub: N) 
  extends ComposableElement[N]

