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
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.graph.DAG
import scala.language.existentials

trait ComposableXYDecomposition[N <: ComposableSDD[N]] extends XYDecomposition[N]{

  // do not refer back to SDD nodes or manager that could refer to this object
  
  //TODO check whether unique nodes cache is broken by reference to vtree here (and parent references in vtree).
  
  //TODO consider sorting elements and associating a designated unique nodes and apply cache with the first prime!
  
  def elements: Seq[ComposableElement[N]]
  
  def +(prime:N, sub: N): ComposableXYDecomposition[N] = {
    if(prime.isConsistent) new MyDecomp(new ComposableElementImpl(prime,sub) +: elements)
    else this
  }
  
  def mapPrimes(f: N => N): ComposableXYDecomposition[N] = {
    new MyDecomp(elements.map(_.mapPrime(f)).flatten)
  }
  
  def mapSubs(f: N => N): ComposableXYDecomposition[N] = {
    new MyDecomp(elements.map(_.mapSub(f)))
  }
  
  def &&(that: ComposableXYDecomposition[N]): ComposableXYDecomposition[N] = {
    val elemConjoin = for(e1 <- this.elements; e2 <- that.elements) yield e1 && e2
    val elems = elemConjoin.flatten
    new MyDecomp(elems)
  }
  
  // specialize instead of using mapSubs because negation cannot undo compression
  def unary_! = mapSubs(!_)
  
  private class MyDecomp(val elements: Seq[ComposableElement[N]]) extends ComposableXYDecomposition[N]
  
}

trait ComposableElement[N <: ComposableSDD[N]] extends Element[N] {
  
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

class ComposableElementImpl[N <: ComposableSDD[N]](val prime: N, val sub: N) 
  extends ComposableElement[N]

