package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTree


trait Normalized extends SDD with Circuit[Normalized]

trait NormalizedDecision[+N <: Normalized] extends DecisionNode[N] with Normalized {
  
  self: N =>
    
  def vtree: VTreeINode[T] forSome { type T <: VTree[T] }
    
  assume(primes.forall{_.respects(vtree.vl)}, "no vtree nodes are skipped")
  assume(subs.forall{_.respects(vtree.vr)}, "no vtree nodes are skipped")
  
}

trait NormalizedTerminal extends TerminalNode with Normalized {
  
  def vtree: VTreeLeaf[T] forSome { type T <: VTree[T] }
  
}
