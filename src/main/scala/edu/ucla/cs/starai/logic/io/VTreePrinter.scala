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