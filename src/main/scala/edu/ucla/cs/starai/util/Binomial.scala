package edu.ucla.cs.starai.util

import scala.language.implicitConversions

object Binomial {

  private[this] val factorialCache = new collection.mutable.ArrayBuffer[LogDouble] ++ List(LogDouble.one, LogDouble.one)

  def factorial(n: Int): LogDouble = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): LogDouble = factorial(n) / factorial(k) / factorial(n - k)

}