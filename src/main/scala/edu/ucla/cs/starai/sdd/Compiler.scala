package edu.ucla.cs.starai.sdd

import edu.ucla.cs.starai.logic.Clause
import edu.ucla.cs.starai.logic.DimacsCNF
import edu.ucla.cs.starai.logic.VTree
import edu.ucla.cs.starai.sdd.manager.ManagedSDDWithKey
import edu.ucla.cs.starai.sdd.manager.SDDManager
import edu.ucla.cs.starai.sdd.manager.SDDManagerINode
import edu.ucla.cs.starai.sdd.manager.SDDManagerLeaf

trait Compiler {
  
  def compile(cnf: DimacsCNF): ManagedSDD = compile(cnf,VTree.balanced(cnf.numVars))
  
  def compile(cnf: DimacsCNF, vtree: VTree): ManagedSDD = compile(cnf,SDDManager(vtree))
  
  def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD
  
  def compileClause(clause: Clause, mgr: SDDManager): ManagedSDD = {
    clause.literals.foldLeft(mgr.False){(c,l) => c || mgr.literal(l)}
  }
  
  def reportApplies(mgr: SDDManager) {
    val stats = mgr.inodes.map { x => (x.numApplies,x.variables.size) }.toSeq
    val avgAppliesPerLevel = stats.groupBy(_._2).mapValues(stats => stats.map(_._1).sum)
    for((level,applies) <- avgAppliesPerLevel.toSeq.sortBy(_._1)){
      println(s"($applies total applies on $level variables)")
    }
  }
  
}


class NaiveCompiler extends Compiler {
  
  override def compile(cnf: DimacsCNF, mgr: SDDManager): ManagedSDD = {
    // do this in a way that allows for garbage collection
    val numClauses = cnf.numClauses
    var i = 0;
    var cnfSDD: ManagedSDD = mgr.True
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
  
    def getRelevantClauses(mgr: SDDManager) = {
      val mgrVariables = mgr.variables
      val (mgrClauses,otherClauses) = clauses.partition { _.variables subsetOf mgrVariables }
      clauses = otherClauses
      mgrClauses
    }
  
    def input(leaf: SDDManagerLeaf): ManagedSDDWithKey = {
      getRelevantClauses(leaf).foldLeft[ManagedSDDWithKey](leaf.True){
        (cnf,cl) => 
          i = i+1
          println(s"(${(i*100)/numClauses}%, size ${cnf.sddSize}) Compiling clause $cl ")
          (cnf && compileClause(cl,leaf))
      }
    }
    
    def propagate(inode: SDDManagerINode, childCnfs: Seq[ManagedSDDWithKey]): ManagedSDDWithKey = {
      reportApplies(mgr)
      assume(childCnfs.size == 2)
      val cnfX = inode.lift(childCnfs(0))
      val cnfY = inode.lift(childCnfs(1))
      println(s"(${(i*100)/numClauses}%, size ${cnfX.sddSize},${cnfY.sddSize}) Compiling aggregate CNF ")
      val cnfXY = cnfX && cnfY
      val myCnf = getRelevantClauses(inode).foldLeft[ManagedSDDWithKey](cnfXY){
        (cnf,cl) => 
          i = i+1
          reportApplies(mgr)
          println(s"(${(i*100)/numClauses}%, size ${cnf.sddSize}) Compiling clause $cl ")
          (cnf && compileClause(cl,inode))
      }
      myCnf
    }
    
    mgr.foldUp(input, propagate)
  }
  
}