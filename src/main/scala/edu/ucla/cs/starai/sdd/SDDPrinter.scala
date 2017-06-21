package edu.ucla.cs.starai.sdd

import java.io.PrintStream

import scala.collection.Seq

import edu.ucla.cs.starai.sdd

final class SDDPrinter(output: PrintStream, verbose: Boolean = false) {

  def print(node: SDD) {
    
    val vtreeIds = node.vtree.toIndexedSeq.zipWithIndex.toMap
    var id = 0
    
    def input(leaf: SDDLeaf) = {
      val vtreeId = vtreeIds(leaf.vtree)
      leaf match {
        case leaf: TrueLeaf    => output.println(s"T $id $vtreeId")
        case leaf: FalseLeaf   => output.println(s"F $id $vtreeId")
        case leaf: LiteralLeaf => output.println(s"L $id $vtreeId ${leaf.literal}")
      }
      id = id + 1
      s"$id"
    }
    
    def propagate(inode: SDDINode, childIDs: Seq[String]) = {
      val vtreeId = vtreeIds(inode.vtree)
      inode match {
        case _: SDDDecNode => {
          output.println(s"D $id $vtreeId ${childIDs.size} ${childIDs.mkString(" ")}")
          id = id + 1
          s"$id"
        }
        case _: SDDElemNode => { childIDs.mkString(" ") }
        case _              => throw new IllegalArgumentException()
      }
    }
    
    node.foldUp(input, propagate)
    
  }

}