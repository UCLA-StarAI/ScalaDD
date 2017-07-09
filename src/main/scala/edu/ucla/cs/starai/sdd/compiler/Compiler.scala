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

package edu.ucla.cs.starai.sdd.compiler

import edu.ucla.cs.starai.logic.Clause
import edu.ucla.cs.starai.logic.DimacsCNF
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.sdd.manager.normalized.ManagedSDD
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManagerINode
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManagerLeaf
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManager
import edu.ucla.cs.starai.sdd.ComposableLiteralNode
import com.google.common.collect.Multiset
import com.google.common.collect.Multisets
import com.google.common.collect.HashMultiset
import scala.collection.mutable.ListBuffer


trait Compiler {
  
  def compile(cnf: DimacsCNF): ManagedSDD = compile(cnf,VTree.balanced(cnf.numVars))
  
  def compile(cnf: DimacsCNF, vtree: VTree.Some): ManagedSDD = compile(cnf,SDDManager(vtree))
  
  def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD
  
  def compileClause(clause: Clause, mgr: SDDManager): ManagedSDD = {
    clause.literals.foldLeft(mgr.False){(c,l) => c || mgr.literal(l)}
  }
  
}


class NaiveCompiler(val verbose: Int = 1) extends Compiler {
  
  override def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD = {
    // do this in a way that allows for garbage collection
    val numClauses = cnf.numClauses
    var i = 0;
    var cnfSDD: ManagedSDD = mgr.True
    for(clause <- cnf.clauseLines){
      i = i+1
      if(verbose>0) println(s"(${(i*100)/numClauses}%, size ${cnfSDD.sddSize}) Compiling clause $clause ")
      cnfSDD = cnfSDD && compileClause(clause,mgr)
    }
    cnfSDD
  }
  
}

class TreeCompiler(
    val verbose: Int = 1, 
    val useOrderHeuristic: Boolean = true) 
    extends Compiler {
  
  override def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD = {
    
    val numClauses = cnf.numClauses
    var i = 0;
    var clauses = cnf.clauseLines.toList
  
    def removeRelevantClauses(mgr: SDDManager) = {
      val mgrVariables = mgr.variables
      val (mgrClauses,otherClauses) = clauses.partition { _.variables subsetOf mgrVariables }
      clauses = otherClauses
      mgrClauses
    }  
    
    def propagate(node: SDDManager, childCnfs: Seq[ManagedSDD]): ManagedSDD = node.kind match{
      case Left(leaf) => removeRelevantClauses(leaf).foldLeft[ManagedSDD](leaf.True){
        (cnf,cl) => 
          i = i+1
          if(verbose>0) println(s"(${(i*100)/numClauses}%, size ${cnf.sddSize}) Compiling clause $cl ")
          (cnf && compileClause(cl,leaf))
      }
      case Right(inode) => {
        assume(childCnfs.size == 2)
        val cnfX = childCnfs(0)
        val cnfY = childCnfs(1)
        if(verbose>0) println(s"(${(i*100)/numClauses}%, size ${cnfX.sddSize},${cnfY.sddSize}) Compiling aggregate CNF ")
        var cnfXY = inode.indepConjoin(cnfX,cnfY)
        val relevantClauses = removeRelevantClauses(inode)
        val sortedClauses = if(useOrderHeuristic){ 
          val depths = relevantClauses.flatMap(_.variables).toSet
                        .map{ v: Variable => (v -> inode.depth(v))}.toMap
          relevantClauses.sortBy{c: Clause =>
            c.variables.toSeq.map(depths(_)).sorted.reverse
          }(scala.math.Ordering.Implicits.seqDerivedOrdering)
        } else relevantClauses
        for(clause <- sortedClauses){
          i = i+1
          if(verbose>0) println(s"(${(i*100)/numClauses}%, size ${cnfXY.sddSize}) Compiling clause $clause")
          cnfXY = (cnfXY && compileClause(clause,inode))
        }
        cnfXY
      }
    }
    // decorate output in case one expects a normalized SDD for the root
    mgr.normalize(mgr.foldUp(propagate))
  }
  
}