package edu.ucla.cs.starai.sdd.manager

import edu.ucla.cs.starai.util.UniqueCache
import edu.ucla.cs.starai.util.Conversions._
import com.google.common.cache.CacheStats
import java.util.concurrent.atomic.AtomicLong
import com.google.common.cache.CacheBuilder
import com.google.common.cache.Cache
import scala.math._

trait UniqueNodes extends (Seq[ManagedSDDElemNode] => ManagedSDDDecNode){
  
  def cacheSize: Long
  
}

// make sure cache is symmetric wrt arguments
// make sure cache keys cannot reference cache values, or auto-GC is broken!
class UniqueNodesCache(mgr: SDDManagerINode) extends UniqueNodes {
    
  val nextDecompKey = new AtomicLong(0)
  
  protected def createNew(elems: Set[ManagedSDDElemNode]): ManagedSDDDecNode = {
    new ManagedSDDDecNode(nextDecompKey.getAndIncrement,elems.toSeq)
  }
  
  type Key = Set[(Long,Long)]
  
  val cache: Cache[Key,ManagedSDDDecNode] = CacheBuilder
    .newBuilder
    .weakValues()
  //    .maximumSize(10000)
    .build()
    
  private def makeCacheKey(elems: Set[ManagedSDDElemNode]): Key = {
    assume(elems.map{_.sub}.toSet.size == elems.size,s"Compress before generating keys: $elems")
    elems.map{e => (e.prime.key,e.sub.key)}
  }
  
  def apply(elems: Seq[ManagedSDDElemNode]): ManagedSDDDecNode = {
    val compressedElems = compress(elems) 
    val result = cache.get(makeCacheKey(compressedElems), () => {createNew(compressedElems.toSet)})
    result
  }
  
  // Helper functions
  
  private def compress(elems: Seq[ManagedSDDElemNode]): Set[ManagedSDDElemNode] = {
	  val primesBySub = elems.filter(_.prime.isConsistent).groupBy(_.sub ).mapValues { _.map(_.prime) }
	  val primeBySub = primesBySub.mapValues { _.reduce { (p1, p2) => p1 || p2 } }
	  primeBySub.map{case (sub,prime) => ManagedSDDElemNode(mgr,prime,sub)}.toSet
  }
  
  def cacheSize: Long = cache.size
  
  // currently does nothing because stats are not kept
  def cacheStats: CacheStats = cache.stats
  
  override def toString = cache.asMap.toString()
  
}

class PermanentUniqueNodesCache(mgr: SDDManagerINode) extends UniqueNodesCache(mgr) {
  
  override val cache: Cache[Key,ManagedSDDDecNode] = CacheBuilder
    .newBuilder
    .build()
  
}


class UniqueNodesCacheKeepFirst(mgr: SDDManagerINode, numNodesToKeep: Int) extends UniqueNodesCache(mgr) {
  
  val hardRefs = Array.ofDim[ManagedSDDDecNode](numNodesToKeep)
  
  override protected def createNew(elems: Set[ManagedSDDElemNode]): ManagedSDDDecNode = {
    val created = super[UniqueNodesCache].createNew(elems)
    if(created.key < numNodesToKeep) hardRefs(created.key.toInt) = created
    created
  }
  
}