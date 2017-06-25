//package edu.ucla.cs.starai.sdd.manager
//
//import edu.ucla.cs.starai.logic.Literal
//import edu.ucla.cs.starai.graph.DAG
//import edu.ucla.cs.starai.graph.LeafNode
//import edu.ucla.cs.starai.graph.INode
//import edu.ucla.cs.starai.sdd.SDD
//import edu.ucla.cs.starai.sdd.SDDINode
//import edu.ucla.cs.starai.sdd.SDDLeaf
//import edu.ucla.cs.starai.sdd.TrueNode
//import edu.ucla.cs.starai.logic.VTreeLeaf
//import edu.ucla.cs.starai.sdd._
//import edu.ucla.cs.starai.sdd.FalseNode
//import edu.ucla.cs.starai.sdd.ElementNode
//import edu.ucla.cs.starai.sdd.DecisionNode
//import edu.ucla.cs.starai.sdd.ElementNode
//
//
//class ManagedTrueLeaf(val manager: SDDManagerLeaf) extends ManagedSDDLeaf with TrueNode  {
//  def unary_! = manager.False
//  def &&(other: ManagedSDD) = {
//    assume(other.manager == this.manager)
//    other.asInstanceOf[ManagedSDDWithKey]
//  }
//  def ||(other: ManagedSDD) = {
//    assume(other.manager == this.manager)
//    this
//  }
//  def |(l: Literal) = manager.literal(l)
//  def key = 1
//}
//
//class ManagedFalseLeaf(val manager: SDDManagerLeaf) extends ManagedSDDLeaf with FalseNode  {
//  def unary_! = manager.True
//  
//  def &&(other: ManagedSDD) = {
//    assume(other.manager == this.manager)
//    this
//  }
//  def ||(other: ManagedSDD) = {
//    assume(other.manager == this.manager)
//    other.asInstanceOf[ManagedSDDWithKey]
//  }
//  def |(l: Literal) = this
//  def key = 0
//}
//
//trait ManagedLiteralLeaf extends ManagedSDDLeaf with LiteralNode  {
//  def |(l: Literal) = if(literal == l) manager.True else manager.False
//}
//
//class ManagedPositiveLiteralLeaf(val manager: SDDManagerLeaf) extends ManagedLiteralLeaf {
//  def unary_! = manager.NegativeLiteral
//  def literal = vtree.variable
//  def &&(other: ManagedSDD): ManagedSDDLeaf = other match {
//    case manager.True => this
//    case manager.False => manager.False
//    case manager.PositiveLiteral => this
//    case manager.NegativeLiteral => manager.False
//    case _ => throw new IllegalArgumentException
//  }
//  def ||(other: ManagedSDD): ManagedSDDLeaf = other match {
//    case manager.True => manager.True
//    case manager.False => this
//    case manager.PositiveLiteral => this
//    case manager.NegativeLiteral => manager.True
//    case _ => throw new IllegalArgumentException
//  }
//  def key = 3
//}
//
//class ManagedNegativeLiteralLeaf(val manager: SDDManagerLeaf) extends ManagedLiteralLeaf {
//  def unary_! = manager.PositiveLiteral
//  def literal = !vtree.variable
//  def &&(other: ManagedSDD): ManagedSDDLeaf = other match {
//    case manager.True => this
//    case manager.False => manager.False
//    case manager.PositiveLiteral => manager.False
//    case manager.NegativeLiteral => this
//    case _ => throw new IllegalArgumentException
//  }
//  def ||(other: ManagedSDD): ManagedSDDLeaf = other match {
//    case manager.True => manager.True
//    case manager.False => this
//    case manager.PositiveLiteral => manager.True
//    case manager.NegativeLiteral => this
//    case _ => throw new IllegalArgumentException
//  }
//  def key = 4
//}
//
