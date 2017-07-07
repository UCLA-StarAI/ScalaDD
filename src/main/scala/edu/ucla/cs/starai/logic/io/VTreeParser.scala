/*
 * Copyright 2017 Guy Van den Broeck <guyvdb@cs.ucla.edu>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.logic.io

import scala.io.Source

import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.logic.VTreeINodeImpl
import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic.Variable
import edu.ucla.cs.starai.logic.VTreeImpl


/**
 * A parser for .vtree files.
 **/
class VTreeParser(verbosity: Int = 0) {
  
  // used for IO
  // TODO clean up, use proper parser
  
  def parse(src: Source): VTree.Some = {
    //c ids of vtree nodes start at 0
    //c ids of variables start at 1
    //c vtree nodes appear bottom-up, children before parents
    var l = 0;
    val lines = src.getLines().toArray
    val (comments, code) = lines.partition { _.startsWith("c") }
    if (verbosity>0) {
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
    val vtreeNodes = Array.ofDim[VTreeImpl](nbNodes)
    var lastNode: VTreeImpl = null // needed because node index may not be bottom-up
    for (linei <- 0 until nbNodes) {
      if (verbosity>0 ) println("Reading VTree file "+(linei*100/nbNodes)+"%")
      val line = code(1 + linei).split(" ").toList
      if (verbosity>0 ) println("Parsing line "+l+": " + line); l+=1
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