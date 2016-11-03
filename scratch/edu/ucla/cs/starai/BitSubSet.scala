package edu.ucla.cs.starai;

import edu.ucla.cs.starai.util.BitSubSet

object BitSubSetScratch extends App {
  
  val s = BitSubSet(Array("a", "c", "d"))
  println(s)
  val t = s - "c"
  println(t)
  val u = t + "c"
  println(u)
  val v = u.filter(_ != "d")
  println(v)
  println(v.size)
  val w = v intersect t
  println(w)
  val x = v union t
  println(x)
  
}

