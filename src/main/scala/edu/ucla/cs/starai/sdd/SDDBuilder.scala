package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree

/**
 * A builder of SDDs (encapsulating the right vtree to use).
 */
trait SDDBuilder[N <: SDD]{
    
  /**
   * Builds a representation of the given SDD according to the conventions of this manager.
   * Use case: take an SDD for a sub-vtree and lift it to the vtree of this manager.
   */
  def decorate(sdd: N): N
  
  /**
   * Mutually exclusive and exhaustive primes with their subs
   */
  def buildPartition(primes: Seq[N],subs: Seq[N]): N
  
  /**
   * Decomposable conjunction. 
   * Arguments are in any order but are assumed to respect a lower vtree.
   */
  def buildDecomposition(x: N, y: N): N
  
  /**
   * Requires that l.variable is in vtree
   */
  def buildLiteral(l: Literal): N
  def buildFalse(): N
  def buildTrue(): N
  
}

trait BuilderVTree[N <: SDD] extends DoubleLinkedTree[BuilderVTree[N]] 
  with VTree[BuilderVTree[N]] 
  with SDDBuilder[N] {
  
  
}