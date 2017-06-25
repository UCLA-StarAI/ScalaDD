package edu.ucla.cs.starai.logic.io

import scala.io.Source
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.VTreeLeaf
import edu.ucla.cs.starai.logic.Variable
import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic.VTreeINodeImpl


/**
 * A parser for .vtree files.
 **/
class VTreeParser(verbosity: Int = 0) {
  
  // used for IO
  // TODO clean up, use proper parser
  
  def parse(src: Source): VTree[_] = {
    //c ids of vtree nodes start at 0
    //c ids of variables start at 1
    //c vtree nodes appear bottom-up, children before parents
    var l = 0;
    val lines = src.getLines().toArray
    val (comments, code) = lines.partition { _.startsWith("c") }
    if (verbosity>1) {
      println("Comments:")
      println(comments.mkString("\n"))
      println
      println("Code:")
      println(code.mkString("\n"))
      println
    }
    require(lines.size > 0, "No code lines.")
    //c vtree number-of-nodes-in-vtree
    val vtreeLine = code.head.split(" ").toList
    require(vtreeLine.size == 2, "Invalid: " + code.head)
    require(vtreeLine.head == "vtree", "Line should start with 'vtree': " + code.head)
    val nbNodes = vtreeLine(1).toInt
    val vtreeNodes = Array.ofDim[VTree[_]](nbNodes)
    var lastNode: VTree = null // needed because node index may not be bottom-up
    for (linei <- 0 until nbNodes) {
      if (verbosity>1 ) println("Reading VTree file "+(linei*100/nbNodes)+"%")
      val line = code(1 + linei).split(" ").toList
      println("Compiling line "+l+": " + line); l+=1
      line match {
        //c L id-of-leaf-vtree-node id-of-variable
        case "L" :: tail => {
          val intTail = tail.map { _.toInt }
          intTail match {
            case nodeId :: variable :: Nil => {
              vtreeNodes(nodeId) = new VTreeLeafImpl(variable)
              lastNode = vtreeNodes(nodeId)
            }
            case _ => require(false, "Cannot parse " + line)
          }
        }
        //c I id-of-internal-vtree-node id-of-left-child id-of-right-child
        case "I" :: tail => {
          val intTail = tail.map { _.toInt }
          intTail match {
            case nodeId :: leftId :: rightId :: Nil => {
              val vtree = new VTreeINodeImpl(vtreeNodes(leftId), vtreeNodes(rightId))
              vtreeNodes(nodeId) = vtree
              lastNode = vtree
            }
            case _ => require(false, "Cannot parse " + line)
          }
        }
        case Nil => {}
        case _ => require(false, "Cannot parse " + line)
      }
    }
    src.close
    lastNode
  }

}