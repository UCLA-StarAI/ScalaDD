package edu.ucla.cs.starai.util

import scala.language.implicitConversions

object KLD {

  import SignLogDouble._
  
  def asymmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    if (p.isZero) {
      zero
    } else if (q.isZero) {
    	// smooth
        asymmetricKld(p, 0.000001)
    } else {
    	p * (p / q).log
    }
  }

  def symmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    val r1 = asymmetricKld(p, q)
    val r2 = asymmetricKld(q, p)
    (r1 + r2)
  }
  
  def symmetricKld(p: SignLogDouble, q: SignLogDouble, r: SignLogDouble): SignLogDouble = {
    val r1 = symmetricKld(p, q)
    val r2 = symmetricKld(q, r)
    val r3 = symmetricKld(p, r)
    (r1 + r2 + r3)
  }

}