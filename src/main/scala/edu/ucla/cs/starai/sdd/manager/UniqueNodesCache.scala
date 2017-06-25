//package edu.ucla.cs.starai.sdd.manager
//
//import java.util.concurrent.atomic.AtomicLong
//
//import com.google.common.cache.Cache
//import com.google.common.cache.CacheBuilder
//import com.google.common.cache.CacheStats
//
//trait UniqueNodes extends (Seq[ManagedSDDElemNodeImpl] => ManagedSDDDecNodeImpl){
//  
//  def cacheSize: Long
//  
//}
//
//// make sure cache is symmetric wrt arguments
//// make sure cache keys cannot reference cache values, or auto-GC is broken!
//class UniqueNodesCache(mgr: SDDManagerINode) extends UniqueNodes {
//    
//  val nextDecompKey = new AtomicLong(0)
//  
//  protected def createNew(elems: Set[ManagedSDDElemNodeImpl]): ManagedSDDDecNodeImpl = {
//    new ManagedSDDDecNodeImpl(nextDecompKey.getAndIncrement,elems.toSeq)
//  }
//  
//  type Key = Set[(Long,Long)]
//  
//  val cache: Cache[Key,ManagedSDDDecNodeImpl] = CacheBuilder
//    .newBuilder
//    .weakValues()
//  //    .maximumSize(10000)
//    .build()
//    
//  private def makeCacheKey(elems: Set[ManagedSDDElemNodeImpl]): Key = {
//    assume(elems.map{_.sub}.toSet.size == elems.size,s"Compress before generating keys: $elems")
//    elems.map{e => (e.prime.key,e.sub.key)}
//  }
//  
//  def apply(elems: Seq[ManagedSDDElemNodeImpl]): ManagedSDDDecNodeImpl = {
//    val compressedElems = compress(elems) 
//    val result = cache.get(makeCacheKey(compressedElems), () => {createNew(compressedElems.toSet)})
//    result
//  }
//  
//  // Helper functions
//  
//  private def compress(elems: Seq[ManagedSDDElemNodeImpl]): Set[ManagedSDDElemNodeImpl] = {
//	  val primesBySub = elems.filter(_.prime.isConsistent).groupBy(_.sub ).mapValues { _.map(_.prime) }
//	  val primeBySub = primesBySub.mapValues { _.reduce { (p1, p2) => p1 || p2 } }
//	  primeBySub.map{case (sub,prime) => ManagedSDDElemNodeImpl(mgr,prime,sub)}.toSet
//  }
//  
//  def cacheSize: Long = cache.size
//  
//  // currently does nothing because stats are not kept
//  def cacheStats: CacheStats = cache.stats
//  
//  override def toString = cache.asMap.toString()
//  
//}
//
//class PermanentUniqueNodesCache(mgr: SDDManagerINode) extends UniqueNodesCache(mgr) {
//  
//  override val cache: Cache[Key,ManagedSDDDecNodeImpl] = CacheBuilder
//    .newBuilder
//    .build()
//  
//}
//
//
//class UniqueNodesCacheKeepFirst(mgr: SDDManagerINode, numNodesToKeep: Int) extends UniqueNodesCache(mgr) {
//  
//  val hardRefs = Array.ofDim[ManagedSDDDecNodeImpl](numNodesToKeep)
//  
//  override protected def createNew(elems: Set[ManagedSDDElemNodeImpl]): ManagedSDDDecNodeImpl = {
//    val created = super[UniqueNodesCache].createNew(elems)
//    if(created.key < numNodesToKeep) hardRefs(created.key.toInt) = created
//    created
//  }
//  
//}