package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Clause
import edu.ucla.cs.starai.logic.DimacsCNF
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.sdd.manager.normalized.ManagedSDD
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManagerINode
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManagerLeaf
import edu.ucla.cs.starai.sdd.manager.normalized.SDDManager

trait Compiler {
  
  def compile(cnf: DimacsCNF): ManagedSDD = compile(cnf,VTree.balanced(cnf.numVars))
  
  def compile(cnf: DimacsCNF, vtree: VTree.SomeVtree): ManagedSDD = compile(cnf,SDDManager(vtree))
  
  def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD
  
  def compileClause(clause: Clause, mgr: SDDManager): ManagedSDD = {
    clause.literals.foldLeft(mgr.buildFalse){(c,l) => c || mgr.buildLiteral(l)}
  }
  
}


class NaiveCompiler extends Compiler {
  
  override def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD = {
    // do this in a way that allows for garbage collection
    val numClauses = cnf.numClauses
    var i = 0;
    var cnfSDD: ManagedSDD = mgr.buildTrue
    for(clause <- cnf.clauseLines){
      i = i+1
      println(s"(${(i*100)/numClauses}%, size ${cnfSDD.sddSize}) Compiling clause $clause ")
      cnfSDD = cnfSDD && compileClause(clause,mgr)
    }
    cnfSDD
  }
  
}

class TreeCompiler extends Compiler {
  
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
      case Left(leaf) => removeRelevantClauses(leaf).foldLeft[ManagedSDD](leaf.buildTrue){
        (cnf,cl) => 
          i = i+1
          println(s"(${(i*100)/numClauses}%, size ${cnf.sddSize}) Compiling clause $cl ")
          (cnf && compileClause(cl,leaf))
      }
      case Right(inode) => {
        assume(childCnfs.size == 2)
        val cnfX = childCnfs(0)
        val cnfY = childCnfs(1)
        println(s"(${(i*100)/numClauses}%, size ${cnfX.sddSize},${cnfY.sddSize}) Compiling aggregate CNF ")
        val cnfXY = inode.buildDecomposition(cnfX,cnfY)
        val myCnf = removeRelevantClauses(inode).foldLeft[ManagedSDD](cnfXY){
          (cnf,cl) => 
            i = i+1
            println(s"(${(i*100)/numClauses}%, size ${cnf.sddSize}) Compiling clause $cl ")
            (cnf && compileClause(cl,inode))
        }
        myCnf
      }
    }
    // decorate output in case one expects a normalized SDD for the root
    mgr.decorate(mgr.foldUp(propagate))
  }
  
}