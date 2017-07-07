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

import scala.io.Source

import org.scalatest.FlatSpec

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.logic.io.VTreeParser
import edu.ucla.cs.starai.sdd.compiler.TreeCompiler

class FileCompilationSpec extends FlatSpec with SDDBehaviors {
  
  val vtreeParser = new VTreeParser
  val cnfParser = DimacsIO
  val compiler = new TreeCompiler(0)
  
  {
    behavior of "CNF cm151a_mince"
    val cnf = cnfParser.parse(Source.fromResource("cnfs/easy/cm151a_mince.cnf"))
    val vtree = vtreeParser.parse(Source.fromResource("cnfs/easy/cm151a_mince.balanced.vtree"))
    val sdd = compiler.compile(cnf, vtree)
    it should behave like correctUsedVarModelCount(sdd,4096)
  }
  
  {
    behavior of "CNF count_mince"
    val cnf = DimacsIO.parse(Source.fromResource("cnfs/easy/count_mince.cnf"))
    val vtree = vtreeParser.parse(Source.fromResource("cnfs/easy/count_mince.min.vtree"))
    val sdd = compiler.compile(cnf, vtree)
    it should behave like correctUsedVarModelCount(sdd,34359738368L)
  }
    
}