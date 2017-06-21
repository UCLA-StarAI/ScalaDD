package edu.ucla.cs.starai.util

import scala.util.hashing.MurmurHash3._

/**
 * A class with a proxy AnyVal key for caching computations without referencing the original input objects
 * Important for garbage collecting computations on these objects when using caching.
 */
trait ProxyKey[Key] {
  def key: Key
}

/**
 * Keys that are invariant to permutations of the elements
 */
sealed trait SymmetricKey[K]

object SymmetricKey {
  
  val seed = "SymmetricKey".hashCode
  def from[K](x: ProxyKey[K], y: ProxyKey[K]) = new SymmetricKey2(x.key,y.key)
  // for higher arities, it's better to reuse Set
  
}

final class SymmetricKey2[K](val x: K, val y: K) extends SymmetricKey[K] {
  
  override def equals(that: Any): Boolean = {
      try {
        val thatKey = that.asInstanceOf[SymmetricKey2[K]]
        ((thatKey.x == x) && (thatKey.y == y)) || ((thatKey.x == y) && (thatKey.y == x))
      }catch { case ex: ClassCastException => false }
  }
    
  /**
   * Optimized version of scala.util.hashing.MurmurHash3.setHash
   * No need to cache, it is called exactly once by the hashmap
   */
  override def hashCode() = {
        var a, b, n = 0
        var c = 1;
        { 
          val h = x.##
          a += h
          b ^= h
          if (h != 0) c *= h
          n += 1
        }
        { 
          val h = y.##
          a += h
          b ^= h
          if (h != 0) c *= h
          n += 1
        }
        var h = SymmetricKey.seed
        h = mix(h, a)
        h = mix(h, b)
        h = mixLast(h, c)
        finalizeHash(h, n)
  }
  
  override def toString = s"($x,$y)"
  
}
