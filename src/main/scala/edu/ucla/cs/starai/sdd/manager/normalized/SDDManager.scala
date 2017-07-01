package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.sdd.BuilderVTree
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.Compressed
import edu.ucla.cs.starai.sdd.Normalized
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.TerminalNode
import edu.ucla.cs.starai.sdd.DecisionNode
import edu.ucla.cs.starai.sdd.TrueNode
import edu.ucla.cs.starai.sdd.FalseNode
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache


trait SDDManager extends VTree[SDDManager] with BuilderVTree[ManagedSDD]

trait SDDManagerLeaf extends SDDManager with VTreeLeaf[SDDManager] {
  
  private abstract class MyTerminal extends ManagedTerminal{
    def vtree = SDDManagerLeaf.this
  }
  
  val buildTrue: ManagedTerminal = new MyTerminal with ManagedTrue
  val buildFalse: ManagedTerminal = new MyTerminal with ManagedFalse
  val posLit: ManagedTerminal = new MyTerminal with ManagedLiteral{
    def literal = vtree.variable
  }
  posLit.sddSize
  val negLit: ManagedTerminal = new MyTerminal with ManagedLiteral{
    def literal = !vtree.variable
  }
  
  def buildLiteral(l: Literal) = {
     if(this.variable == l.variable) {
       if(l.isPositive) posLit
       else negLit
     }else throw new IllegalArgumentException(s"$this cannot build literal $l, only for $variable")
  }
  
  def buildPartition(primes: Seq[ManagedSDD],subs: Seq[ManagedSDD]): ManagedDecision = {
    throw new IllegalArgumentException(s"$this cannot build partitions")
  }
  
  def buildDecomposition(x: ManagedSDD, y: ManagedSDD): ManagedDecision = {
    throw new IllegalArgumentException(s"$this cannot build decompositions")
  }
  
  def decorate(sdd: ManagedSDD): ManagedSDD = {
    if(sdd.vtree == this) return sdd
    else throw new IllegalArgumentException(s"$this cannot build sdds for other managers")
  }
  
}

trait SDDManagerINode extends SDDManager with VTreeINode[SDDManager] {
  
  def uniqueNodesCache: UniqueNodesCache[ManagedSDD]
  
  private class MyDecision(val primes: Seq[ManagedSDD], val subs: Seq[ManagedSDD]) 
    extends ManagedDecision {
    def vtree = SDDManagerINode.this
  }
  
  private[this] val truePrimes = Seq(vl.buildTrue)
  private[this] val trueSubs = Seq(vr.buildTrue)
  private[this] val falseSubs = Seq(vr.buildFalse)
  private[this] val trueFalseSubs = Seq(vr.buildTrue,vr.buildFalse)
  
  val buildTrue = uniqueNodesCache.getOrBuild(truePrimes, trueSubs,
      new MyDecision(truePrimes,trueSubs) with ManagedTrue)
      
  val buildFalse = uniqueNodesCache.getOrBuild(truePrimes, falseSubs,
      new MyDecision(truePrimes,falseSubs) with ManagedFalse)
      
      
  override def buildLiteral(l: Literal) = literalCache(l)
      
  protected val literalCache: Map[Literal,ManagedSDD] = (
       vl.literals.map{ l => 
         val primes = Seq(vl.buildLiteral(l),!vl.buildLiteral(l))
         (l-> uniqueNodesCache.getOrBuild(primes, trueFalseSubs,
           new MyDecision(primes,trueFalseSubs) with ManagedLiteral {
             def literal = l
           }))
       } ++ 
       vr.literals.map{ l => 
         val subs = Seq(vr.buildLiteral(l))
         (l->uniqueNodesCache.getOrBuild(truePrimes, subs,
           new MyDecision(truePrimes,subs) with ManagedLiteral {
             def literal = l
           }))
       }).toMap
  
  def buildPartition(primes: Seq[ManagedSDD], subs: Seq[ManagedSDD]): ManagedSDD = {
    val compressed = subs.hasDistinctElements
    val consistentPrimes = primes.forall(_.isConsistent)
    val (newPrimes,newSubs) = 
      if(compressed && consistentPrimes) (primes,subs)
      else {
        // remove inconsistent primes
        val elems = (primes zip subs).filter(_._1.isConsistent)
        // compress
        val primesBySub = elems.groupBy(_._2 ).mapValues { _.map( _._1) }
    	  val primeBySub = primesBySub.mapValues { _.reduce { (p1, p2) => p1 || p2 } 
  	                                              /*TODO: optimize this order*/ }
    	  val (subsIter,primesIter) = primeBySub.unzip
    	  (primesIter.toSeq,subsIter.toSeq)
      }
     uniqueNodesCache.getOrBuild(newPrimes, newSubs, new MyDecision(newPrimes,newSubs))
  }
  
  def buildDecomposition(x: ManagedSDD, y: ManagedSDD): ManagedSDD = {
    val (prime,sub) = if(vl.contains(x.vtree)) (x,y) else (y,x)
    assume(vl.contains(prime.vtree))
    assume(vr.contains(sub.vtree))
    buildPartition(Seq(prime,!prime),Seq(sub,vr.buildFalse))
  }
  
  def decorate(sdd: ManagedSDD): ManagedSDD = {
    if(sdd.vtree == this) {
      return sdd
    } else if(!this.contains(sdd.vtree)){
      throw new IllegalArgumentException(s"$this cannot build sdds for other managers")
    } else if(vl.contains(sdd.vtree)){
      buildPartition(Seq(sdd,!sdd),trueFalseSubs)
    }else{
      assume(vr.contains(sdd.vtree))
      buildPartition(truePrimes,Seq(sdd))
    }
  }
  
}