package edu.ucla.cs.starai.util

/**
 * Extends Either to three elements
 */
sealed trait Either3[+L,+M,+R]

case class Left3[+L,+M,+R](l: L) extends Either3[L,M,R]
case class Mid3[+L,+M,+R](m: M) extends Either3[L,M,R]
case class Right3[+L,+M,+R](r: R) extends Either3[L,M,R]