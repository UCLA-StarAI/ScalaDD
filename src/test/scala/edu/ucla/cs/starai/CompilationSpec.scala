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

import org.scalatest.FlatSpec
import scala.language.existentials
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManager

class CompilationSpec extends FlatSpec with SDDBehaviors {

  behavior of "A balanced Vtree of 8 variables"

  val vtree = VTree.balanced(8)
  
  it should "have 8 variables" in {
    assert(vtree.variables.size === 8)
  }
    
  val mgr = SDDManager(vtree)
  
  behavior of "A buildFalse SDD"
  it should behave like correctSize(mgr.False,7)
  it should behave like correctModelCount(mgr.False,0)
  
  behavior of "A buildTrue SDD"
  it should behave like correctSize(mgr.True,7)
  it should behave like correctModelCount(mgr.True,scala.math.pow(2,8).toInt)
  
  val x1 = mgr.literal(1)
  val x2 = mgr.literal(2)
  val x3 = mgr.literal(3)
  val x4 = mgr.literal(4)
  val x5 = mgr.literal(5)
  val x6 = mgr.literal(6)
  val x7 = mgr.literal(7)
  val x8 = mgr.literal(8)
  
  behavior of "A clause"
  val c1 = !x1 ||  x2 || !x5
  it should behave like correctSize(c1,22)
  it should behave like correctModelCount(c1,(BigInt(2).pow(8)-BigInt(2).pow(8-3)).toInt)
    
  behavior of "Another clause"
  val c2 =  x1 || !x3 ||  x6
  it should behave like correctSize(c2,24)
  it should behave like correctModelCount(c2,(BigInt(2).pow(8)-BigInt(2).pow(8-3)).toInt)
  
  val c3 = !x2 ||  x4 ||  x7
  val c4 =  x2 ||  x7 || !x8
  val c5 = !x3 || !x4 ||  x6
  val c6 =  x4 || !x6 ||  x8
  val c7 =  x3 ||  x4 || !x8
  val c8 = !x5 ||  x6 || !x7
  val c9 =  x6 ||  x7 || !x8
  
  behavior of "A CNF"
  val cnf = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9
  it should behave like correctSize(cnf,122)
  it should behave like correctModelCount(cnf,84)
  
  behavior of "An inconsistent SDD"
  val unsat = cnf && !c5
  it should behave like correctSize(unsat,7)
  it should behave like correctModelCount(unsat,0)
    
}