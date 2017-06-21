package edu.ucla.cs.starai

import edu.ucla.cs.starai.sdd.SDD
import org.scalatest.FlatSpec

trait SDDBehaviors { this: FlatSpec =>

  def correctSize(sdd: => SDD, size: Int) {
    it should s"have size $size" in {
      assert(sdd.sddSize === size)
    }
  }
  
  def correctModelCount(sdd: => SDD, modelCount: Int) {
    it should s"have model count $modelCount" in {
      assert(sdd.modelCount === modelCount)
    }
  }

}