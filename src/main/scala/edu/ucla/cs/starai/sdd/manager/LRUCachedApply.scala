//package edu.ucla.cs.starai.sdd.manager
//
//import com.google.common.cache.Cache
//import com.google.common.cache.CacheBuilder
//
//import edu.ucla.cs.starai.sdd.ManagedSDDDecNode
//import edu.ucla.cs.starai.util.SymmetricKey
//
//

//    
//
//// make sure cache is symmetric wrt arguments
//// make sure cache keys cannot reference cache values, or auto-GC is broken!
//abstract class LRUCachedSymmetricApply(mgr: SDDManagerINode) extends ApplyFunction {
//  
//   private var numApplies_ = 0;
//  
//   final val falseElem = ManagedSDDElemNodeImpl(mgr,mgr.ml.False, mgr.mr.False)
//  
//   private def computeNew(x: ManagedSDDDecNodeImpl, y: ManagedSDDDecNodeImpl): ManagedSDDDecNodeImpl = {
//          val uncompressedElems = for (e1<-x.elems; e2<-y.elems) yield {
//            val prime = e1.prime && e2.prime
//            if (prime.isConsistent) {
//              val sub = op(e1.sub, e2.sub)
//              if(prime.isValid && sub.isValid) return mgr.True
//              else ManagedSDDElemNodeImpl(mgr,prime,sub)
//            }else falseElem
//          }
//          mgr.uniqueNode(uncompressedElems)
//        }
//  
//    val cache: Cache[SymmetricKey[Long],ManagedSDDDecNodeImpl] = CacheBuilder
//      .newBuilder
//      .softValues()
//  //    .maximumSize(10000)
//      .build()
//    
//    def apply(x: ManagedSDDDecNodeImpl, y: ManagedSDDDecNodeImpl): ManagedSDDDecNodeImpl = {
//      numApplies_ = numApplies_ + 1
//      val key = SymmetricKey.from(x,y)
//      val result = cache.get(key, () => {computeNew(x,y)})
//      assume(result.modelCount > 0 || result == mgr.False, 
//          s"There should be only one False node: $result != ${mgr.False}")
//      assume(result.modelCount < BigInt(2).pow(result.vtree.numVariables) || result == mgr.True, 
//          s"There should be only one True node: $result  != ${mgr.True}")
//      result
//    }
//    
//    def numApplies: Int = numApplies_
//    
//}
//
//
///**
// * Overlay a fast Array cache
// */
//abstract class LRUCachedSymmetricApplyBrute(mgr: SDDManagerINode) extends LRUCachedSymmetricApply(mgr) {
//  
//  val numFunctions = BigInt(2).pow(BigInt(2).pow(mgr.vtree.numVariables).toInt).toInt
//  val bruteCache: Array[Array[ManagedSDDDecNodeImpl]] = Array.ofDim(numFunctions, numFunctions)
//  
//  override def apply(x: ManagedSDDDecNodeImpl, y: ManagedSDDDecNodeImpl): ManagedSDDDecNodeImpl = {
//    assume(x.key.toInt < numFunctions, s"Too many unique nodes: ${mgr.uniqueNode}")
//    assume(y.key.toInt < numFunctions, s"Too many unique nodes: ${mgr.uniqueNode}")
//    val cachedBrute = bruteCache(x.key.toInt)(y.key.toInt)
//    if(cachedBrute != null) return cachedBrute
//    else{
//      val computed = super.apply(x, y);
//      bruteCache(x.key.toInt)(y.key.toInt) = computed
//      bruteCache(y.key.toInt)(x.key.toInt) = computed
//      return computed
//    }
//  }
//}
//
//
//
///**
// * Overlay a fast Array cache
// */
//abstract class LRUCachedSymmetricApplyPartialBrute(mgr: SDDManagerINode, cacheFirstN: Int) 
//  extends LRUCachedSymmetricApply(mgr) {
//  
//  val bruteCache: Array[Array[ManagedSDDDecNodeImpl]] = Array.ofDim(cacheFirstN, cacheFirstN)
//  
//  override def apply(x: ManagedSDDDecNodeImpl, y: ManagedSDDDecNodeImpl): ManagedSDDDecNodeImpl = {
//    if(x.key < cacheFirstN && y.key < cacheFirstN){
//      val cachedBrute = bruteCache(x.key.toInt)(y.key.toInt)
//      if(cachedBrute != null) return cachedBrute
//      else{
//        val computed = super[LRUCachedSymmetricApply].apply(x, y);
//        bruteCache(x.key.toInt)(y.key.toInt) = computed
//        bruteCache(y.key.toInt)(x.key.toInt) = computed
//        return computed
//      }
//    }else super.apply(x, y);
//  }
//}