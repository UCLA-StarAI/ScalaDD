package edu.ucla.cs.starai;

import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeParser
import java.io.File
import scala.io.Source

object VtreeParserScratch extends App {
  
  val parser = new VTreeParser(1)
  val vtree = parser.parse(Source.fromFile("examples/big-swap.vtree"))
  
  println(s"Size = ${vtree.size}")
  println(s"Vars = ${vtree.variables}")
  println(s"Left size = ${vtree.asInstanceOf[VTreeINode].vl.size}")
  println(s"Right size = ${vtree.asInstanceOf[VTreeINode].vr.size}")
  
}

