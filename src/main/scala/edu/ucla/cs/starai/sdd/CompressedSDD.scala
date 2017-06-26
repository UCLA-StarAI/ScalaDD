package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Circuit
import edu.ucla.cs.starai.logic.VTreeLeaf


trait Compressed extends SDD with Circuit[Compressed]

trait CompressedDecision extends DecisionNode with Compressed {
  
  assume(subs.distinctElements, "Compressed SDDs cannot have repeated subs")
    
  def elems: Seq[CompressedElement]
  override def children: Seq[CompressedElement] = elems
}

trait CompressedElement extends ElementNode with Compressed {
  
  def prime: Compressed
  def sub: Compressed
  
  override def children: Seq[Compressed] = Seq(prime,sub)
  
}

trait CompressedLeaf extends SDDLeaf with Compressed {
  
  def vtree: VTreeLeaf[_]
  
}
