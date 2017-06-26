package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf


trait Normalized extends SDD with Circuit[Normalized]

trait NormalizedDecision extends DecisionNode with Normalized {
  
  assume(primes.forall{_.subRespects(vtree.vl)}, "no vtree nodes are skipped")
  assume(subs.forall{_.subRespects(vtree.vr)}, "no vtree nodes are skipped")
    
  def elems: Seq[NormalizedElement]
  override def children: Seq[NormalizedElement] = elems
}

trait NormalizedElement extends ElementNode with Normalized {
  
  def prime: Normalized
  def sub: Normalized
  
  override def children: Seq[Normalized] = Seq(prime,sub)
  
}

trait NormalizedLeaf extends SDDLeaf with Normalized {
  
  def vtree: VTreeLeaf[_]
  
}
