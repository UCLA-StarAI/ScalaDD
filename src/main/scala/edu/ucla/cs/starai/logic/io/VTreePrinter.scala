package edu.ucla.cs.starai.logic.io

import scala.collection._
import java.io._
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf

class VTreePrinter(output: PrintStream) {

  def print(node: VTree[_]) {
    var id = 0
    def input(leaf: VTreeLeaf[_]) = {
      output.println("L " + id + " " + leaf.variable + " 0")
      id = id+1
      id
    }
    def propagate(inode: VTreeINode[_], childIDs: Seq[Int]) = {
      output.println("I " + id + " " + childIDs.mkString(" ") + " 0")
      id = id+1
      id
    }
    node.foldUp(input, propagate)
  }

}