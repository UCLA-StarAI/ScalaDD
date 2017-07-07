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

trait XYDecomposition[+N <: SDD] extends Caching[DAG[_]]{
  
  def elements: Seq[Element[N]]    
  
  def children: Seq[N] = elements.flatMap(_.children)
  def primes: Seq[N] = elements.map(_.prime)
  def subs: Seq[N] = elements.map(_.sub)
  
  def size = elements.size
          
  def isConsistent(cache: Cache[Boolean]) = elements.exists(_.isConsistent(cache))
  
  def isSubTrimmableFirst = (size == 2 && elements(0).sub.isValid && !elements(1).sub.isConsistent)
  def isSubTrimmableSecond = (size == 2 && elements(1).sub.isValid && !elements(0).sub.isConsistent)
  def isSubTrimmable = (isSubTrimmableFirst || isSubTrimmableSecond)
  def isPrimeTrimmable = (size == 1 && elements(0).prime.isValid)
  def isTrimmable = isPrimeTrimmable || isSubTrimmable
  
  def modelRatio(cache: Cache[BigRational]) = elements.map(_.modelRatio(cache)).sum

  override def toString = elements.mkString("XY{", ", ", "}")
  
  override lazy val hashCode = {
    scala.util.hashing.MurmurHash3.unorderedHash(elements)
  }
  
  final override def equals(that:Any) = that match {
    case that:XYDecomposition[N] => {
     (this eq that) ||
      (this.elements.size == that.elements.size) && {
       val myElems = elements.toSet
       that.elements.forall(myElems.contains(_))
     }
    }
    case _ => false
  }
}


trait Element[+N <: SDD] extends Caching[DAG[_]] {
  
  def prime: N 
  def sub: N
  
  assume(prime.isConsistent)
  
  def isConsistent: Boolean = isConsistent(emptyCache)
  def isConsistent(cache: Cache[Boolean]): Boolean = 
    prime.isConsistent(cache) && sub.isConsistent(cache)
  
  def modelRatio(cache: Cache[BigRational]) = prime.modelRatio(cache) * sub.modelRatio(cache)
  
  def subRespects(vtree: VTreeINode.Some ): Boolean = prime.subRespects(vtree.vl) && sub.subRespects(vtree.vr)
  
  def children: Seq[N] = Seq(prime,sub)

  override def toString = s"[$prime,$sub]"

  final override lazy val hashCode = {
    // taken from MurmurHash3.productHash for performance
    import MurmurHash3._
    val seed = 0xcafebabe 
    var h = seed
    h = mix(h, prime.##)
    h = mix(h, sub.##)
    finalizeHash(h, 2)
  }
  
  final override def equals(o:Any) = o match {
    case e:Element[N] => (prime == e.prime) && (sub == e.sub)
    case _ => false
  }
}