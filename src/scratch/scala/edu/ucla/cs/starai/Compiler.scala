package edu.ucla.cs.starai;

import java.io.File
import edu.ucla.cs.starai.sdd.TreeCompiler
import edu.ucla.cs.starai.logic.VTreeParser
import edu.ucla.cs.starai.logic.DimacsIO
import scala.io.Source

object CompilerScratch extends App {
    
  val parser = new VTreeParser(1)
  
//  val cnf = DimacsIO.parse("examples/s208.1.scan.cnf")
  
//  val cnf = DimacsIO.parse("examples/count.cnf")
//  val vtree = parser.parse(new File("examples/count.cnf.vtree"))
    
  val cnf = DimacsIO.parse(Source.fromResource("cnfs/count-short.cnf"))
  val vtree = parser.parse(Source.fromResource("cnfs/count-short.cnf.vtree"))
  
//  val cnf = DimacsIO.parse("examples/c8.cnf")"
  
//  val cnf = DimacsIO.parse("examples/c432.isc.cnf")
  
//  val cnf = DimacsIO.parse("examples/big-swap.cnf")
//  val vtree = parser.parse(new File("examples/big-swap.cnf.vtree"))
  
  
//  val compiler = new NaiveCompiler
  val compiler = new TreeCompiler
  
  val sdd = compiler.compile(cnf, vtree)
  
  
  println(s"Number of variables = ${cnf.numVars}")
  println(s"Number of clauses = ${cnf.numClauses}")
  println(s"Number of vtree nodes = ${vtree.numNodes}")
  println(s"Number of vtree variables = ${vtree.numVariables}")
  
  println(s"SDD used vars = ${sdd.usedVars}")
  println(s"SDD used vars number = ${sdd.usedVars.size}")
  
  println(s"SDD size = ${sdd.sddSize}")
  println(s"SDD nodes = ${sdd.sddNodes}")
  
  println(s"Trimmed SDD size = ${sdd.trimmedSddSize}")
  println(s"Trimmed SDD nodes = ${sdd.trimmedSddNodes}")
  
  println(s"Interpretation count = ${BigInt(2).pow(cnf.numVars)}")
  println(s"SDD model count = ${sdd.modelCount}")
  println(s"SDD used model count = ${sdd.usedVarsModelCount}")
  
  println(s"Manager unique nodes cache size = ${sdd.manager.uniqueNodesCacheSize}")
  
}

