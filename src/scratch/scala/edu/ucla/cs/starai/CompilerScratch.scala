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

import java.io.File
import edu.ucla.cs.starai.logic.io.VTreeParser
import edu.ucla.cs.starai.logic.DimacsIO
import scala.io.Source
import edu.ucla.cs.starai.sdd.compiler.TreeCompiler
import edu.ucla.cs.starai.util._

object CompilerScratch extends App {
    
    assertFalse
  
  val vtreeParser = new VTreeParser(1)
  
  val suite = "iscas89"
  val benchmark = "s832.scan" 
  
//  val suite = "easy"
//  val benchmark = "count_mince" 
  
  val vtreeType = "min"
//  val vtreeType = "balanced"
  
  val cnf = DimacsIO.parse(Source.fromResource(s"cnfs/$suite/$benchmark.cnf"))
              .simplify(0.51)
  val vtree = vtreeParser.parse(Source.fromResource(s"cnfs/$suite/$benchmark.$vtreeType.vtree"))
  
//  val compiler = new NaiveCompiler
  val compiler = new TreeCompiler(1,true)
  
  val sdd = time("Compilation"){compiler.compile(cnf, vtree)}

//  println(s"Number of variables = ${cnf.numVars}")
//  println(s"Number of clauses = ${cnf.numClauses}")
//  println(s"Number of vtree nodes = ${vtree.numNodes}")
//  println(s"Number of vtree variables = ${vtree.numVariables}")
//  
//  println(s"SDD used vars = ${sdd.usedVars}")
//  println(s"SDD used vars number = ${sdd.usedVars.size}")
  
  println(s"SDD size = ${sdd.sddSize}")
//  println(s"SDD nodes = ${sdd.numNodes}")
  
//  println(s"Trimmed SDD size = ${sdd.trimmedSize}")
//  println(s"Trimmed SDD number of nodes = ${sdd.trimmedNumNodes}")
//  println(sdd.trim)
  
//  println(s"Interpretation count = ${BigInt(2).pow(cnf.numVars)}")
//  println(s"SDD model count = ${sdd.modelCount}")
//  println(s"SDD used model count = ${sdd.usedVarsModelCount}")
  
//  println(s"Manager unique nodes cache size = ${sdd.vtree.uniqueNodesCache}")
    
}

