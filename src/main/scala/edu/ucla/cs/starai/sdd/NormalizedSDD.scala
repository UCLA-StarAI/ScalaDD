package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.logic.VTreeINode


trait Normalized extends SDD with Circuit[Normalized]

trait NormalizedDecision[+N <: Normalized] extends DecisionNode[N] with Normalized {
  
  self: N =>
    
  def vtree: VTreeINode[_]
    
  assume(primes.forall{_.subRespects(vtree.vl)}, "no vtree nodes are skipped")
  assume(subs.forall{_.subRespects(vtree.vr)}, "no vtree nodes are skipped")
  
}

trait NormalizedLeaf extends SDDLeaf with Normalized {
  
  def vtree: VTreeLeaf[_]
  
}
