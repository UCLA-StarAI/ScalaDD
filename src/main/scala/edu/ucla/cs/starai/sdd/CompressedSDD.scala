package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf

trait Compressed extends SDD with Circuit[Compressed]

trait CompressedDecision[+N <: Compressed] extends DecisionNode[N] with Compressed {
  
  self: N =>
    
  assume(subs.hasDistinctElements, "Compressed SDDs cannot have repeated subs")
  
}
