/*
 * Copyright 2017 Guy Van den Broeck
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

package edu.ucla.cs.starai.sdd.io

import java.io.PrintStream
import edu.ucla.cs.starai.sdd.TrueNode
import edu.ucla.cs.starai.sdd.FalseNode
import edu.ucla.cs.starai.sdd.SDD
import edu.ucla.cs.starai.sdd.DecisionNode
import edu.ucla.cs.starai.sdd.LiteralNode
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.util.Mid3
import edu.ucla.cs.starai.util.Left3
import edu.ucla.cs.starai.util.Right3
import java.util.concurrent.atomic.AtomicInteger

final class SDDPrinter(output: PrintStream, verbose: Boolean = false) {

  def print(node: SDD) {
    val vtreeIds: Map[VTree[_],Int] = 
      node.vtree.iterator.toIndexedSeq.zipWithIndex.toMap
    var nodeId = 0
    def propagate(node: SDD, childIDs: Seq[String]) = {
      val vtreeId = vtreeIds(node.vtree)
      node.kind match {
        case Right(decision) => {
          output.println(s"D $nodeId $vtreeId ${childIDs.size} ${childIDs.mkString(" ")}")
        }
        case Left(terminal) => terminal.terminalKind match{
          case Left3(x) => output.println(s"L $nodeId $vtreeId ${x.literal}")
          case Mid3(x) => output.println(s"T $nodeId $vtreeId")
          case Right3(x) => output.println(s"F $nodeId $vtreeId")
        }  
      }
      nodeId = nodeId + 1
      nodeId.toString
    }
    node.foldUp(propagate)
  }

}