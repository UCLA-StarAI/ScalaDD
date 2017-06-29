package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.DoubleLinkedTree

/**
 * A builder of SDDs (encapsulating the right vtree to use).
 */
trait SDDBuilder[N <: SDD]{
    
  def build(sdd: N): N
  
  /**
   * Mutually exclusive and exhaustive primes with their subs
   */
  def buildPartition(primes: Seq[N],subs: Seq[N]): DecisionNode[N] with N
  
  /**
   * Decomposable conjunction. Arguments are in any order.
   */
  def buildDecomposition(x: N, y: N): DecisionNode[N] with N
  
  /**
   * Requires that l.variable is in vtree
   */
  def buildLiteral(l: Literal): LiteralNode with N
  def buildFalse(): FalseNode with N
  def buildTrue(): TrueNode with N
  
}

trait BuilderVTree[N <: SDD] extends DoubleLinkedTree[BuilderVTree[N]] 
  with VTree[BuilderVTree[N]] 
  with SDDBuilder[N] {
  
  
  
}