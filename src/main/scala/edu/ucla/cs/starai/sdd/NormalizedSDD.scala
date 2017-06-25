package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf


trait NormalizedSDD extends SDD with Circuit[NormalizedSDD]


trait NormalizedDecision extends DecisionNode with NormalizedSDD {
  
  assume(primes.forall{_.subRespects(vtree.vl)},"no vtree nodes are skipped")
  assume(subs.forall{_.subRespects(vtree.vr)},"no vtree nodes are skipped")
    
  def elems: Seq[NormalizedElement]
  override def children: Seq[NormalizedElement] = elems
}

trait NormalizedElement extends ElementNode with NormalizedSDD {
  
  def prime: NormalizedSDD
  def sub: NormalizedSDD
  
  override def children: Seq[NormalizedSDD] = Seq(prime,sub)
  
}

trait NormalizedLeaf extends SDDLeaf with NormalizedSDD {
  
  def vtree: VTreeLeaf[_]
  
}


