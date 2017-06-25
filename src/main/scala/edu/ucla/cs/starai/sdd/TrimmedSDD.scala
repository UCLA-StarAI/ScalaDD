package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit


trait TrimmedSDD extends SDD with Circuit[TrimmedSDD] {
  
}

trait TrimmedDecisionNode extends DecisionNode with TrimmedSDD {
  
  
  def elems: Seq[ElementNode with TrimmedSDD]
  override def children: Seq[ElementNode with TrimmedSDD] = elems
  
}

trait TrimmedDecision extends DecisionNode with TrimmedSDD {
  
  assume(!isTrimmable,"Trimmed SDDs cannot have trimmable nodes")
  
  def elems: Seq[TrimmedElement]
  override def children: Seq[TrimmedElement] = elems
}

trait TrimmedElement extends ElementNode with TrimmedSDD {
  
  def prime: TrimmedSDD
  def sub: TrimmedSDD
  
  override def children: Seq[TrimmedSDD] = Seq(prime,sub)
  
}


