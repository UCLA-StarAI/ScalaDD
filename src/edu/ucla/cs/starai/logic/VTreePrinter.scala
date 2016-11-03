package edu.ucla.cs.starai.logic

import scala.collection._
import java.io._

class VTreePrinter(output: PrintStream) {

  def print(node: VTree) {
    var id = 0
    def input(leaf: VTreeLeaf) = {
      output.println("L " + id + " " + leaf.variable + " 0")
      id = id+1
      id
    }
    def propagate(inode: VTreeINode, childIDs: Seq[Int]) = {
      output.println("I " + id + " " + childIDs.mkString(" ") + " 0")
      id = id+1
      id
    }
    node.foldUp(input, propagate)
  }

}