package edu.ucla.cs.starai.sdd.manager

import edu.ucla.cs.starai.logic.Literal
import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.SDDINode
import edu.ucla.cs.starai.sdd.SDDLeaf
import edu.ucla.cs.starai.sdd.TrueLeaf
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.sdd._
import edu.ucla.cs.starai.sdd.FalseLeaf
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.sdd.SDDDecNode
import edu.ucla.cs.starai.sdd.SDDElemNode


class ManagedTrueLeaf(val manager: SDDManagerLeaf) extends ManagedSDDLeaf with TrueLeaf[ManagedSDD]  {
  def unary_! = manager.False
  def &&(other: ManagedSDD) = {
    assume(other.manager == this.manager)
    other.asInstanceOf[ManagedSDDWithKey]
  }
  def ||(other: ManagedSDD) = {
    assume(other.manager == this.manager)
    this
  }
  def |(l: Literal) = manager.literal(l)
  def key = 1
}

class ManagedFalseLeaf(val manager: SDDManagerLeaf) extends ManagedSDDLeaf with FalseLeaf[ManagedSDD]  {
  def unary_! = manager.True
  
  def &&(other: ManagedSDD) = {
    assume(other.manager == this.manager)
    this
  }
  def ||(other: ManagedSDD) = {
    assume(other.manager == this.manager)
    other.asInstanceOf[ManagedSDDWithKey]
  }
  def |(l: Literal) = this
  def key = 0
}

trait ManagedLiteralLeaf extends ManagedSDDLeaf with LiteralLeaf[ManagedSDD]  {
  def |(l: Literal) = if(literal == l) manager.True else manager.False
}

class ManagedPositiveLiteralLeaf(val manager: SDDManagerLeaf) extends ManagedLiteralLeaf {
  def unary_! = manager.NegativeLiteral
  def literal = vtree.variable
  def &&(other: ManagedSDD): ManagedSDDLeaf = other match {
    case manager.True => this
    case manager.False => manager.False
    case manager.PositiveLiteral => this
    case manager.NegativeLiteral => manager.False
    case _ => throw new IllegalArgumentException
  }
  def ||(other: ManagedSDD): ManagedSDDLeaf = other match {
    case manager.True => manager.True
    case manager.False => this
    case manager.PositiveLiteral => this
    case manager.NegativeLiteral => manager.True
    case _ => throw new IllegalArgumentException
  }
  def key = 3
}

class ManagedNegativeLiteralLeaf(val manager: SDDManagerLeaf) extends ManagedLiteralLeaf {
  def unary_! = manager.PositiveLiteral
  def literal = !vtree.variable
  def &&(other: ManagedSDD): ManagedSDDLeaf = other match {
    case manager.True => this
    case manager.False => manager.False
    case manager.PositiveLiteral => manager.False
    case manager.NegativeLiteral => this
    case _ => throw new IllegalArgumentException
  }
  def ||(other: ManagedSDD): ManagedSDDLeaf = other match {
    case manager.True => manager.True
    case manager.False => this
    case manager.PositiveLiteral => manager.True
    case manager.NegativeLiteral => this
    case _ => throw new IllegalArgumentException
  }
  def key = 4
}

