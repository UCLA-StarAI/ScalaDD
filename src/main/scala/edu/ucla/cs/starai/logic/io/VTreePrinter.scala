package edu.ucla.cs.starai.logic.io

import scala.collection._
import java.io._
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.logic.VTreeLeaf

class VTreePrinter(output: PrintStream) {

  def print[N <: VTree[N]](node: N) {
    var id = 0
    def propagate(node: VTree[N], childIDs: Seq[Int]): Int = {
      node match {
        case leaf: VTreeLeaf[N] => output.println("L " + id + " " + leaf.variable + " 0")
        case _ => output.println("I " + id + " " + childIDs.mkString(" ") + " 0")
      }
      id = id+1
      id
    }
    node.foldUp[Int](propagate)
  }

}