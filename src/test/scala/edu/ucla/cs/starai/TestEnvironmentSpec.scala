package edu.ucla.cs.starai;

import org.scalatest.FlatSpec

import edu.ucla.cs.starai.logic._

class TestEnvironmentSpec extends FlatSpec {

  behavior of "The test environment"  
  it should "have assertions enabled" in {
    assertThrows[AssertionError] {
      util.assertFalse
    }
  }
    
}