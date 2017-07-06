/*
 * Copyright 2017 Guy Van den Broeck
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

package edu.ucla.cs.starai

import edu.ucla.cs.starai.sdd.SDD
import org.scalatest.FlatSpec

trait SDDBehaviors { this: FlatSpec =>

  def correctSize(sdd: => SDD, size: Int) {
    it should s"have size $size" in {
      assert(sdd.sddSize === size)
    }
  }
  
  def correctModelCount(sdd: => SDD, modelCount: BigInt) {
    it should s"have model count $modelCount" in {
      assert(sdd.modelCount === modelCount)
    }
  }

  def correctUsedVarModelCount(sdd: => SDD, modelCount: BigInt) {
    it should s"have used var model count $modelCount" in {
      assert(sdd.usedVarsModelCount === modelCount)
    }
  }

}