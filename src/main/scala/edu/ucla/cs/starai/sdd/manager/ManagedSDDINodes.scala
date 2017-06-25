//package edu.ucla.cs.starai.sdd.manager
//
//import edu.ucla.cs.starai.logic.Literal
//import edu.ucla.cs.starai.sdd.ManagedSDD
//import edu.ucla.cs.starai.sdd.ManagedSDDINode
//import edu.ucla.cs.starai.sdd.DecisionNode
//import edu.ucla.cs.starai.sdd.ElementNode
//import edu.ucla.cs.starai.sdd.ManagedSDDElemNode
//import edu.ucla.cs.starai.sdd.ManagedSDDDecNode
//
//
//// elements are case classes to automated equality and hashcode
//final case class ManagedSDDElemNodeImpl(
//      val manager: SDDManagerINode, 
//      val prime: ManagedSDDWithKey, 
//      val sub: ManagedSDDWithKey)
//    extends ManagedSDDElemNode {
//  
//  assume(prime!=null)
//  assume(sub!=null)
//  
//  def unary_! = ManagedSDDElemNodeImpl(manager, prime, !sub)
//  
//  // this element may not have a consistent prime!
//  def isConsistent = prime.isConsistent && sub.isConsistent
//  def isValid = prime.isValid && sub.isValid
//  
//  override def children = Seq(prime,sub)
//  
//  def toManagedSDDDecNodeImpl = manager.uniqueNode(Seq(this))
//  
//  def &&(other: ManagedSDD) = {
//    assume(other.isInstanceOf[ManagedSDDDecNodeImpl])
//    toManagedSDDDecNodeImpl && other.asInstanceOf[ManagedSDDDecNodeImpl]
//  }
//  
//  def ||(other: ManagedSDD) = {
//    assume(other.isInstanceOf[ManagedSDDDecNodeImpl])
//    toManagedSDDDecNodeImpl || other.asInstanceOf[ManagedSDDDecNodeImpl]
//  }
//  
//  def |(l: Literal) = (toManagedSDDDecNodeImpl | l)
//    
//  def rewire(x: ManagedSDD, y: ManagedSDD): ManagedSDDElemNodeImpl = {
//    ManagedSDDElemNodeImpl(manager,x.asInstanceOf[ManagedSDDWithKey],y.asInstanceOf[ManagedSDDWithKey])
//  }
//  
//  def toLocalString = s"[${prime.name},${sub.name}]"
//    
//} 
//  
//
//final class ManagedSDDDecNodeImpl(val key: Long, val elems: Seq[ManagedSDDElemNodeImpl]) 
//    extends ManagedSDDDecNode  with ManagedSDDWithKey{
//
//  //assume(false, "Assumptions should be turned off when no debugging")
//  assume(elems.forall { _.manager == elems.head.manager }, "XY-Partitions should have elements from the same mananger: " + elems)
//  assume(elems.map { _.sub }.toSet.size == partitionSize, "XY-Partitions should be compressed: " + elems)
//  assume(elems.forall {_.prime.isConsistent}, "XY-Partitions should have consistent primes: " + elems)
//  
//  def manager = elems.head.manager
//  
//  override def children = elems.toSeq
//  
//         
//  def isConsistent = (this != manager.False)
//  def isValid = (this == manager.True)
//
//  // negation
//  lazy val unary_! = manager.uniqueNode(elems.map{ elem => !elem })
//
//  // conditioning
//  override def |(l: Literal): ManagedSDDDecNodeImpl = manager.|(this, l)
//  
//  // conjunction
//  override def &&(other: ManagedSDD): ManagedSDDDecNodeImpl = {
//    assume(other.manager == this.manager)
//    assume(other.isInstanceOf[ManagedSDDDecNodeImpl])
//    if (this == manager.True) other.asInstanceOf[ManagedSDDDecNodeImpl]
//    else if (this == manager.False) manager.False
//    else if (this == !other) manager.False
//    else if (other == manager.True) this
//    else if (other == manager.False) manager.False
//    else if (other == this) this
//    else manager.&&(this, other.asInstanceOf[ManagedSDDDecNodeImpl])
//  }
//
//  // disjunction
//  override def ||(other: ManagedSDD): ManagedSDDDecNodeImpl = {
//    assume(other.manager == this.manager)
//    assume(other.isInstanceOf[ManagedSDDDecNodeImpl])
//    if (this == manager.True) manager.True
//    else if (this == manager.False) other.asInstanceOf[ManagedSDDDecNodeImpl]
//    else if (this == !other) manager.True
//    else if (other == manager.True) manager.True
//    else if (other == manager.False) this
//    else if (other == this) this
//    else manager.||(this, other.asInstanceOf[ManagedSDDDecNodeImpl])
//  }
//  
//  def rewire(es: Seq[ManagedSDD]): ManagedSDDDecNodeImpl = manager.uniqueNode(es.map(_.asInstanceOf[ManagedSDDElemNodeImpl]))
//  
//  // this seems to speed things up slightly for no reason
//  override def equals(that: Any): Boolean = (that.asInstanceOf[AnyRef] eq this)
//  
//  // gets called thousands of times
//  override def hashCode = System.identityHashCode(this);
//  
//  override def toString = inodes.collect{case dec:ManagedSDDDecNodeImpl => dec.toLocalString}.mkString("\n")
//  
//  def toLocalString = elems.map(_.toLocalString).mkString(s"$name = (", ",", ")")
//  
//}
