package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic._

/**
 * A builder of SDDs (encapsulating the right vtree to use).
 */
trait SDDBuilder{
  
  def build(sdd: ComposableSDD): ComposableSDD
  
  /**
   * Disjoint mutually exclusive elements
   */
  def buildDecision(elems: Seq[ComposableElementNode]): ComposableDecisionNode
  
  /**
   * Conjoin elements in left and right subvtree (order of x and y is irrelevant)
   */
  def buildElement(x: ComposableSDD, y: ComposableSDD): ComposableElementNode
  
  /**
   * Requires that l.variale is in vtree
   */
  def buildLiteral(l: Literal): ComposableLiteralNode
  def buildFalse(): ComposableFalseNode
  def buildTrue(): ComposableTrueNode
  
}