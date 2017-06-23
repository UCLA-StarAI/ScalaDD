package edu.ucla.cs.starai;

import org.scalatest.FlatSpec

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.manager.SDDManager
import scala.io.Source
import edu.ucla.cs.starai.sdd.TreeCompiler

class FileCompilationSpec extends FlatSpec with SDDBehaviors {
  
  val vtreeParser = new VTreeParser
  val cnfParser = DimacsIO
  
  behavior of "Count-Short CNF"
  val cnf = cnfParser.parse(Source.fromResource("cnfs/count-short.cnf"))
  val vtree = vtreeParser.parse(Source.fromResource("cnfs/count.vtree"))
  
  println(s"Number of variables = ${cnf.numVars}")
  println(s"Number of clauses = ${cnf.numClauses}")
  println(s"Number of vtree nodes = ${vtree.numNodes}")
  println(s"Number of vtree variables = ${vtree.numVariables}")
  
  val compiler = new TreeCompiler
  val sdd = compiler.compile(cnf, vtree)
  
//  it should behave like correctSize(sdd,7)
//  it should behave like correctModelCount(sdd,0)
    
}