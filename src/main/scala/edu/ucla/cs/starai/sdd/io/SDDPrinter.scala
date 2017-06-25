package edu.ucla.cs.starai.sdd.io

import java.io.PrintStream
import scala.collection.Seq
import edu.ucla.cs.starai.sdd.FalseNode
import edu.ucla.cs.starai.sdd.LiteralNode
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.DecisionNode
import edu.ucla.cs.starai.sdd.ElementNode
import edu.ucla.cs.starai.sdd.SDDINode
import edu.ucla.cs.starai.sdd.SDDLeaf
import edu.ucla.cs.starai.sdd.TrueNode

final class SDDPrinter(output: PrintStream, verbose: Boolean = false) {

  def print(node: SDD) {
    
    val vtreeIds = node.vtree.toIndexedSeq.zipWithIndex.toMap
    var id = 0
    
    def input(leaf: SDDLeaf) = {
      val vtreeId = vtreeIds(leaf.vtree)
      leaf match {
        case leaf: TrueNode    => output.println(s"T $id $vtreeId")
        case leaf: FalseNode   => output.println(s"F $id $vtreeId")
        case leaf: LiteralNode => output.println(s"L $id $vtreeId ${leaf.literal}")
      }
      id = id + 1
      s"$id"
    }
    
    def propagate(inode: SDDINode, childIDs: Seq[String]) = {
      val vtreeId = vtreeIds(inode.vtree)
      inode match {
        case _: DecisionNode => {
          output.println(s"D $id $vtreeId ${childIDs.size} ${childIDs.mkString(" ")}")
          id = id + 1
          s"$id"
        }
        case _: ElementNode => { childIDs.mkString(" ") }
        case _              => throw new IllegalArgumentException()
      }
    }
    
    node.foldUp(input, propagate)
    
  }

}