package edu.ucla.cs.starai.sdd.manager

import scala.BigInt
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.annotation.migration
import scala.collection.Iterable
import scala.collection.Iterator
import scala.collection.immutable
import scala.math.BigInt.int2bigInt

import edu.ucla.cs.starai.logic.Literal
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.graph.DAG
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.sdd._
import edu.ucla.cs.starai.sdd.SDDDecNode
import edu.ucla.cs.starai.util.ProxyKey



// elements are case classes to automated equality and hashcode
final case class ManagedSDDElemNode(
      val manager: SDDManagerINode, 
      val prime: ManagedSDDWithKey, 
      val sub: ManagedSDDWithKey)
    extends ManagedSDDINode with SDDElemNode {
  
  assume(prime!=null)
  assume(sub!=null)
  
  def unary_! = ManagedSDDElemNode(manager, prime, !sub)
  
  // this element may not have a consistent prime!
  def isConsistent = prime.isConsistent && sub.isConsistent
  def isValid = prime.isValid && sub.isValid
  
  override def children = Seq(prime,sub)
  
  def toManagedSDDDecNode = manager.uniqueNode(Seq(this))
  
  def &&(other: ManagedSDD) = {
    assume(other.isInstanceOf[ManagedSDDDecNode])
    toManagedSDDDecNode && other.asInstanceOf[ManagedSDDDecNode]
  }
  
  def ||(other: ManagedSDD) = {
    assume(other.isInstanceOf[ManagedSDDDecNode])
    toManagedSDDDecNode || other.asInstanceOf[ManagedSDDDecNode]
  }
  
  def |(l: Literal) = (toManagedSDDDecNode | l)
  
  def toLocalString = s"[${prime.name},${sub.name}]"

} 
  

final class ManagedSDDDecNode(val key: Long, val elems: Seq[ManagedSDDElemNode]) 
    extends ManagedSDDINode with SDDDecNode with ManagedSDDWithKey{

  //assume(false, "Assumptions should be turned off when no debugging")
  assume(elems.forall { _.manager == elems.head.manager }, "XY-Partitions should have elements from the same mananger: " + elems)
  assume(elems.map { _.sub }.toSet.size == partitionSize, "XY-Partitions should be compressed: " + elems)
  assume(elems.forall {_.prime.isConsistent}, "XY-Partitions should have consistent primes: " + elems)
  
  def manager = elems.head.manager
  
  override def children = elems.toSeq
  
  def isTrimmable = (
      (partitionSize == 2 && elems(0).sub.isValid && !elems(1).sub.isConsistent && elems(1).prime == !elems(0).prime)
         || (partitionSize == 2 && elems(1).sub.isValid && !elems(0).sub.isConsistent && elems(0).prime == !elems(1).prime)
         || (partitionSize == 1 && elems(0).prime.isValid))
  
  def isConsistent = (this != manager.False)
  def isValid = (this == manager.True)

  // negation
  lazy val unary_! = manager.uniqueNode(elems.map{ elem => !elem })

  // conditioning
  override def |(l: Literal): ManagedSDDDecNode = manager.|(this, l)
  
  // conjunction
  override def &&(other: ManagedSDD): ManagedSDDDecNode = {
    assume(other.manager == this.manager)
    assume(other.isInstanceOf[ManagedSDDDecNode])
    if (this == manager.True) other.asInstanceOf[ManagedSDDDecNode]
    else if (this == manager.False) manager.False
    else if (this == !other) manager.False
    else if (other == manager.True) this
    else if (other == manager.False) manager.False
    else if (other == this) this
    else manager.&&(this, other.asInstanceOf[ManagedSDDDecNode])
  }

  // disjunction
  override def ||(other: ManagedSDD): ManagedSDDDecNode = {
    assume(other.manager == this.manager)
    assume(other.isInstanceOf[ManagedSDDDecNode])
    if (this == manager.True) manager.True
    else if (this == manager.False) other.asInstanceOf[ManagedSDDDecNode]
    else if (this == !other) manager.True
    else if (other == manager.True) manager.True
    else if (other == manager.False) this
    else if (other == this) this
    else manager.||(this, other.asInstanceOf[ManagedSDDDecNode])
  }
  
  // this seems to speed things up slightly for no reason
  override def equals(that: Any): Boolean = (that.asInstanceOf[AnyRef] eq this)
  // gets called thousands of times
  override def hashCode = System.identityHashCode(this);
  
  override def toString = inodes.collect{case dec:ManagedSDDDecNode => dec.toLocalString}.mkString("\n")
  
  def toLocalString = elems.map(_.toLocalString).mkString(s"$name = (", ",", ")")
  
}
