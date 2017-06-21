package edu.ucla.cs.starai.sdd

import java.io.PrintStream

import scala.collection.Seq

import edu.ucla.cs.starai.sdd

final class SDDPrinter(output: PrintStream, verbose: Boolean = false) {

  def print(node: SDD[_]) {
    
    val vtreeIds = node.vtree.toIndexedSeq.zipWithIndex.toMap
    var id = 0
    
    def input(leaf: SDDLeaf[_]) = {
      val vtreeId = vtreeIds(leaf.vtree)
      leaf match {
        case leaf: TrueLeaf[_]    => output.println(s"T $id $vtreeId")
        case leaf: FalseLeaf[_]   => output.println(s"F $id $vtreeId")
        case leaf: LiteralLeaf[_] => output.println(s"L $id $vtreeId ${leaf.literal}")
      }
      id = id + 1
      s"$id"
    }
    
    def propagate(inode: SDDINode[_], childIDs: Seq[String]) = {
      val vtreeId = vtreeIds(inode.vtree)
      inode match {
        case _: SDDDecNode[_] => {
          output.println(s"D $id $vtreeId ${childIDs.size} ${childIDs.mkString(" ")}")
          id = id + 1
          s"$id"
        }
        case _: SDDElemNode[_] => { childIDs.mkString(" ") }
        case _              => throw new IllegalArgumentException()
      }
    }
    
    node.foldUp(input, propagate)
    
  }

}