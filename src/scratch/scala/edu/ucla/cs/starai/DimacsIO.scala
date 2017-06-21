package edu.ucla.cs.starai;

import edu.ucla.cs.starai.logic.DimacsIO

object DimacsIOScratch extends App {
  
  val cnf = DimacsIO.parse("examples/big-swap.cnf")
  
  println("Parsed CNF:")
  println(cnf)
  
}

