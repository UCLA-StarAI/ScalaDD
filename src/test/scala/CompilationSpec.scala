package org.scalatest.examples.flatspec

import org.scalatest.FlatSpec

import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.sdd.manager.SDDManager

class CompilationSpec extends FlatSpec {

  behavior of "A Vtree"

  val vtree = VTree.balanced(8)
  
  it should "have the right number of variables" in {
    assert(vtree.variables.size === 8)
  }
    
  val mgr = SDDManager(vtree)
  
  behavior of "A false SDD"
  
  it should "have the right size" in {
    assert(mgr.False.sddSize === 7)
  }
  it should "have model count 0" in {
    assert(mgr.False.modelCount === 0)
  }
  
  behavior of "A true SDD"
  
  it should "have the right size" in {
    assert(mgr.True.sddSize === 7)
  }
  it should "have model count 2^8" in {
    assert(mgr.True.modelCount === scala.math.pow(2,8))
  }
      
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
  it should "have the right size" in {
    assert(c1.sddSize === 22)
  }
  it should "have the right model count" in {
    assert(c1.modelCount === (scala.math.pow(2,8)-scala.math.pow(2,8-3)))
  }
  
  behavior of "Another clause"
  val c2 =  x1 || !x3 ||  x6
  it should "have the right size" in {
    assert(c2.sddSize === 24)
  }
  it should "have the right model count" in {
    assert(c2.modelCount === (scala.math.pow(2,8)-scala.math.pow(2,8-3)))
  }
  
  val c3 = !x2 ||  x4 ||  x7
  val c4 =  x2 ||  x7 || !x8
  val c5 = !x3 || !x4 ||  x6
  val c6 =  x4 || !x6 ||  x8
  val c7 =  x3 ||  x4 || !x8
  val c8 = !x5 ||  x6 || !x7
  val c9 =  x6 ||  x7 || !x8
  
  behavior of "A CNF"
  val cnf = c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9
  it should "have the right size" in {
    assert(cnf.sddSize === 122)
  }
  it should "have the right model count" in {
    assert(cnf.modelCount === 84)
  }
  
  behavior of "An inconsistent SDD"
  val unsat = cnf && !c5
  it should "have the right size" in {
    assert(unsat.sddSize === 7)
  }
  it should "have model count 0" in {
    assert(unsat.modelCount === 0)
  }
  
}