package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTree


trait Trimmed extends SDD with Circuit[Trimmed]

trait TrimmedDecision[+N <: Trimmed] extends DecisionNode[N] with Trimmed {
      
  self: N =>
  
  assume(!isTrimmable,"Trimmed SDDs cannot have trimmable nodes")
  
}
