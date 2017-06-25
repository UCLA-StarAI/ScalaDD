package edu.ucla.cs.starai.util;

import language.implicitConversions

class BigRational(n: BigInt, d: BigInt) extends Ordered[BigRational]{
  
  require(d != 0)
  
  private val g = gcd(n.abs, d.abs)
  
  private val Precision = 17
  
  val numer = n / g
  
  val denom = d / g
  
  def this(n: BigInt) = this(n, 1)
  
  def +(that: BigRational): BigRational =
    new BigRational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  
  def +(i: BigInt): BigRational =
    new BigRational(numer + i * denom, denom)
  
  def -(that: BigRational): BigRational =
    new BigRational(
      numer * that.denom - that.numer * denom,
      denom * that.denom)
  
  def -(i: BigInt): BigRational =
    new BigRational(numer - i * denom, denom)
  
  def *(that: BigRational): BigRational =
    new BigRational(numer * that.numer, denom * that.denom)
  
  def *(i: BigInt): BigRational =
    new BigRational(numer * i, denom)
  
  def /(that: BigRational): BigRational =
    new BigRational(numer * that.denom, denom * that.numer)
  
  def /(i: BigInt): BigRational =
    new BigRational(numer, denom * i)
  
  def reciprocal: BigRational =
    new BigRational(denom, numer)
  
  override def toString: String =
    s"$numer/$denom"
  
  def toDouble: Double = {
    def div(d1: BigDecimal, d2: BigDecimal) = // drop down to java.math.BigDecimal
      new BigDecimal(d1.bigDecimal.divide(d2.bigDecimal, Precision, java.math.RoundingMode.DOWN))
    div(BigDecimal(numer), BigDecimal(denom))
      .setScale(Precision).doubleValue
  }
  
  def toExactBigInt = {
    require(denom == 1)
    numer
  }
  
  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == BigInt(0)) a else gcd(b, a % b)
  
  def compare(that: BigRational) = {
    val diffNum = (this-that).numer
    if(diffNum == 0) 0
    else if(diffNum < 0) -1
    else 1
  }
  
  override def hashCode: Int = (numer, denom).hashCode
  
  override def equals(other: Any): Boolean =
    other match {
      case that: BigRational =>
        this.numer == that.numer && this.denom == that.denom ||
          this.numer == 0 && that.numer == 0
      case _ => false
    }
}

object BigRational {
  
  def apply(n: BigInt, d: BigInt) = new BigRational(n,d)
  def apply(n: BigInt) = new BigRational(n,1)
  
  def unapply(b: BigRational): Option[(BigInt, BigInt)] = Some((b.numer, b.denom))
  
  implicit def int2BigRational(i: Int): BigRational = new BigRational(i)
  
  implicit object BigRationalIsFractional extends Numeric[BigRational] with Fractional[BigRational]{
      def plus(x: BigRational, y: BigRational): BigRational = x + y
      def minus(x: BigRational, y: BigRational): BigRational = x - y
      def times(x: BigRational, y: BigRational): BigRational = x * y
      def negate(x: BigRational): BigRational = -x
      def fromInt(x: Int): BigRational = BigRational(x)
      def toInt(x: BigRational): Int = throw new UnsupportedOperationException
      def toLong(x: BigRational): Long = throw new UnsupportedOperationException
      def toFloat(x: BigRational): Float = x.toDouble.toFloat
      def toDouble(x: BigRational): Double = x.toDouble
      def div(x: BigRational, y: BigRational): BigRational = x / y
      def compare(x: BigRational,y: BigRational) = x compare y
  }
}