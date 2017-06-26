package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTree


trait Trimmed extends SDD with Circuit[Trimmed]

trait TrimmedDecision extends DecisionNode with Trimmed {
      
  assume(!isTrimmable,"Trimmed SDDs cannot have trimmable nodes")
  
  def elems: Seq[ElementNode with TrimmedElement]
  override def children: Seq[ElementNode with TrimmedElement] = this.elems
  
}

trait TrimmedElement extends ElementNode with Trimmed {
      
  def prime: SDD with Trimmed
  def sub: SDD with Trimmed
  
  override def children: Seq[SDD with Trimmed] = Seq(prime,sub)
  
}
