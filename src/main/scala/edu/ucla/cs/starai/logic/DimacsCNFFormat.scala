package edu.ucla.cs.starai.logic

import scala.collection._
import scala.io.Source

sealed trait DimacsCNFLine

case class Comment(comment: String) extends DimacsCNFLine {
  override def toString = "c " + comment 
}

object Comment{
  val Line = """^c (.*)""".r
}

case class PLine(numVars: Int, numClauses: Int) extends DimacsCNFLine {
  override def toString = "p cnf " + numVars + " " + numClauses
}

object PLine{
  val Line = """^p cnf (\d+) (\d+)""".r
}

case class Clause(literals: Seq[Literal]) extends DimacsCNFLine {
  def isTautology = literals.exists { v => literals.contains { !v } }
  def variables = literals.map(_.variable).toSet
  override def toString = literals.map{_.toInt}.mkString(" ") + " 0"
}

object Clause{
  // it is impossible to match a variable number of groups :-(
  val Line = """^(.*) 0""".r 
}

case class DimacsCNF(lines: Seq[DimacsCNFLine]) {

  val pline: Option[PLine] = lines.collect{case pline:PLine => pline}.headOption
  val clauseLines = lines.collect { case cl: Clause => cl }
  
  def numVars = pline.map(_.numVars).getOrElse{
      clauseLines.flatMap(_.variables).toSet.size
  }
    
  def numClauses = pline.map(_.numClauses).getOrElse{clauseLines.size}
  
  def isTautology = clauseLines.forall { _.isTautology }

  override def toString = lines.mkString("\n")

}

class DimacsCNFBuilder[VariableObject] {

  private val varMap = new mutable.HashMap[VariableObject, Variable]
  private var lastVar = 0

  def getVar(key: VariableObject): Variable = varMap.getOrElseUpdate(key, {
    lastVar += 1
    lastVar
  })
  
  def declare(key: VariableObject) {
    getVar(key)
  }

  private var clauses: List[Clause] = Nil

  def addClause(posLiterals: Seq[VariableObject], negLiterals: Seq[VariableObject]) {
    val pos: Seq[Literal] = posLiterals.map{ getVar(_).toLiteral } 
    val neg: Seq[Literal] = negLiterals.map{ !getVar(_) }
    val newClause = Clause(pos ++ neg)
    clauses = newClause :: clauses
  }

  def toDimacsCNF: DimacsCNF = {
    val pLine = PLine(lastVar, clauses.size)
    DimacsCNF(pLine :: (clauses.reverse))
  }
}

object DimacsIO {
  
  def parse(fileName: String): DimacsCNF = parse(Source.fromFile(fileName))

  def parse(file: Source): DimacsCNF = {
    require(file != null)
    val lines = file.getLines.map { line => line match {
        case PLine.Line(numVars, numClauses) => PLine(numVars.toInt, numClauses.toInt)
        case Comment.Line(c) => Comment(c)
        case Clause.Line(ints) => {
          val lits: Seq[Literal] = ints.split(" ").map{str => new Literal(str.toInt)}
          Clause(lits)
        }
        case _ => throw new IllegalArgumentException(s"Cannot parse line: $line")
      }
    }.toList
    file.close
    new DimacsCNF(lines)
  }
  
}
