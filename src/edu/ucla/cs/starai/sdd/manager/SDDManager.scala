package edu.ucla.cs.starai.sdd.manager

import scala.collection.GenSet

import com.google.common.cache.CacheBuilder
import com.google.common.cache.Cache
import edu.ucla.cs.starai.util.UniqueCache
import edu.ucla.cs.starai.util.LRUCache
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.sdd.SDDDecNode
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.graph.LeafNode
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.graph.INode
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.sdd.SDDElemNode
import edu.ucla.cs.starai.graph.TreeINode
import edu.ucla.cs.starai.graph.TreeLeaf
import com.google.common.cache.CacheStats
import edu.ucla.cs.starai.util.ProxyKey


trait SDDManager extends Tree[SDDManager,SDDManagerLeaf,SDDManagerINode] {

  def vtree: VTree
  def variables = vtree.variables
  
  def True: ManagedSDDWithKey
  def False: ManagedSDDWithKey
  
  implicit def literal(l: Literal): ManagedSDDWithKey

  def allFunctions: Seq[ManagedSDDWithKey] = {
    val terms = variables.foldLeft[Seq[ManagedSDDWithKey]](Seq(True)){ (partialTerms,v) => 
      partialTerms.flatMap { partialTerm => 
        Seq((partialTerm && v.toLiteral), partialTerm && (!v)) 
      }
    }
    val functions = terms.toSet.subsets().map(_.foldLeft(False)(_ || _))
    functions.toList
  }
  
  def uniqueNodesCacheSize: Long = inodes.map{_.uniqueNode.cacheSize}.sum
  
}

object SDDManager{
  
  def apply(vtree: VTree): SDDManager = {
    def input(leaf: VTreeLeaf) = new SDDManagerLeaf(leaf)
    def propagate(inode: VTreeINode, childManagers: Seq[SDDManager]) = {
      assume(childManagers.size == 2)
      if(inode.numVariables <= 3) new SDDManagerINodeBrute(inode,childManagers(0),childManagers(1))
      else new SDDManagerINodePartialBrute(inode,childManagers(0),childManagers(1))
//      new SDDManagerINodeN(inode,childManagers(0),childManagers(1))
    }
    vtree.foldUp(input, propagate)
  }
  
}

class SDDManagerLeaf(val vtree: VTreeLeaf) extends SDDManager with TreeLeaf[SDDManager,SDDManagerLeaf,SDDManagerINode] {

  def variable = vtree.variable

  implicit def literal(l: Literal): ManagedLiteralLeaf = {
    assume(l.variable == variable)
    if (l.isPositive) PositiveLiteral
    else NegativeLiteral
  }

  val True = new ManagedTrueLeaf(this)
  val False = new ManagedFalseLeaf(this)
  val PositiveLiteral = new ManagedPositiveLiteralLeaf(this)
  val NegativeLiteral = new ManagedNegativeLiteralLeaf(this)
  
  def localUniqueNodesCacheSize: Long = 4
  def localUniqueNodesCacheStats = new CacheStats(0,0,0,0,0,0)
  
}


trait SDDManagerINode extends SDDManager with TreeINode[SDDManager,SDDManagerLeaf,SDDManagerINode] {

  def vtree: VTreeINode
  def ml: SDDManager
  def mr: SDDManager
  
  override def children = Seq(ml,mr)

  implicit def literal(l: Literal) = {
    val v = l.variable
    assume(variables.contains(v))
    if (ml.variables.contains(v)) {
      val mlLiteral = ml.literal(l)
      liftLeft(mlLiteral)
    } else {
      val mrLiteral = mr.literal(l)
      liftRight(mrLiteral)
    }
  }
  
  def lift(node: ManagedSDDWithKey): ManagedSDDDecNode = {
    if(node.manager == ml) liftLeft(node)
    else if(node.manager == mr) liftRight(node)
    else throw new IllegalArgumentException
  }
  
  def liftLeft(node: ManagedSDDWithKey): ManagedSDDDecNode = {
    assume(node.manager == ml)
    if (!node.isConsistent) False
    else if (node.isValid) True
    else uniqueNode(Seq(ManagedSDDElemNode(this,node,mr.True), ManagedSDDElemNode(this,!node, mr.False)))
  }

  def liftRight(node: ManagedSDDWithKey): ManagedSDDDecNode = {
    assume(node.manager == mr)
    uniqueNode(Seq(ManagedSDDElemNode(this,ml.True,node)))
  }

  def uniqueNode: UniqueNodes
  
  //TODO give special toString, make object and register it with the cache 
  def True = uniqueNode(Seq(ManagedSDDElemNode(this,ml.True, mr.True)))
  def False = uniqueNode(Seq(ManagedSDDElemNode(this,ml.True, mr.False)))
  
  def && : ApplyFunction
  
  def || : ApplyFunction
  
  def |(a: ManagedSDDDecNode, l: Literal): ManagedSDDDecNode = {
    if(ml.variables.contains(l.variable)){
      uniqueNode(a.elems.map{case ManagedSDDElemNode(self,p,s) => ManagedSDDElemNode(this,(p|l),s)})
    }else{
      assume(mr.variables.contains(l.variable))
      uniqueNode(a.elems.map{case ManagedSDDElemNode(self,p,s) => ManagedSDDElemNode(this,p,(s|l))})
    }
  }
  
  def numApplies = &&.numApplies + ||.numApplies
  
}

class SDDManagerINodeN(val vtree: VTreeINode, val ml: SDDManager, val mr: SDDManager) 
	extends SDDManagerINode {

  //TODO turn uniqueNode into a mixin trait
  
  lazy val uniqueNode = new UniqueNodesCache(this)
  
  override val True = super.True
  override val False =  super.False
  
  assume(uniqueNode(Seq(ManagedSDDElemNode(this,ml.True, mr.True))) == True)
  assume(uniqueNode(Seq(ManagedSDDElemNode(this,ml.True, mr.False))) == False)
  
  val && = new LRUCachedSymmetricApply(this) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x && y
  }
  
  val || = new LRUCachedSymmetricApply(this) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x || y
  }
  
}


class SDDManagerINodeBrute(vtree: VTreeINode, ml: SDDManager, mr: SDDManager) 
	extends SDDManagerINodeN(vtree,ml,mr) {
  
  // also make sure that gc never happens for the brute cached nodes.
  
  override lazy val uniqueNode = new PermanentUniqueNodesCache(this)
  
  override val && = new LRUCachedSymmetricApplyBrute(this) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x && y
  }
  
  override val || = new LRUCachedSymmetricApplyBrute(this) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x || y
  }
  
}



class SDDManagerINodePartialBrute(vtree: VTreeINode, ml: SDDManager, mr: SDDManager, cacheFirstN: Int = 1000) 
	extends SDDManagerINodeN(vtree,ml,mr) {
  
  // also make sure that gc never happens for the brute cached nodes.
  override lazy val uniqueNode = new UniqueNodesCacheKeepFirst(this, cacheFirstN)
  
  override val && = new LRUCachedSymmetricApplyPartialBrute(this,cacheFirstN) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x && y
  }
  
  override val || = new LRUCachedSymmetricApplyPartialBrute(this,cacheFirstN) {
    override def op(x : ManagedSDDWithKey,y: ManagedSDDWithKey) = x || y
  }
  
}