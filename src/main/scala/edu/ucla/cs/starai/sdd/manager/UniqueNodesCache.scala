package edu.ucla.cs.starai.sdd.manager

import java.util.concurrent.atomic.AtomicLong

import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheStats
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.util._

trait UniqueNodesCache[N <: SDD] {
  
  def cacheSize: Long
  
  def getOrBuild(primes: Seq[N], subs: Seq[N], build: () => N): N
  
}

class GoogleWeakCache[N <: SDD] extends UniqueNodesCache[N] {
    
  // make sure cache is symmetric wrt arguments
  // make sure cache keys cannot reference cache values, or auto-GC is broken!
  private[this] case class Key(elems: Set[(N,N)]){

    def this(primes: Seq[N], subs: Seq[N]){
      this((primes zip subs).toSet)
    }
    
  }
  
  private[this] val cache: Cache[Key,N] = CacheBuilder
    .newBuilder
    .weakValues()
    .build()
    
  def getOrBuild(primes: Seq[N], subs: Seq[N], build: () => N): N = {
    cache.get(new Key(primes,subs), build)
  }
    
  def cacheSize: Long = cache.size
  
  // currently does nothing because stats are not kept
  def cacheStats: CacheStats = cache.stats
  
  override def toString = cache.asMap.toString()
  
}
