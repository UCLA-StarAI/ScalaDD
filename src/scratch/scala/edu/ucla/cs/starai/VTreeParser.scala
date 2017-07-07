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

package edu.ucla.cs.starai;

import edu.ucla.cs.starai.logic.VTreeINode
import edu.ucla.cs.starai.logic.io.VTreeParser
import java.io.File
import scala.io.Source

object VtreeParserScratch extends App {
  
  val parser = new VTreeParser(1)
  val vtree = parser.parse(Source.fromFile("examples/big-swap.vtree"))
  
  println(s"Size = ${vtree.size}")
  println(s"Vars = ${vtree.variables}")
  println(s"Left size = ${vtree.asInstanceOf[VTreeINode[_]].vl.size}")
  println(s"Right size = ${vtree.asInstanceOf[VTreeINode[_]].vr.size}")
  
}

