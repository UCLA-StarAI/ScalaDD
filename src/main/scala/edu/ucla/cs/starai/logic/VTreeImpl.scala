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

package edu.ucla.cs.starai.logic

import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.graph.Tree
import edu.ucla.cs.starai.graph.DoubleLinkedTree
import edu.ucla.cs.starai.sdd.SDD

trait VTreeImpl extends VTree[VTreeImpl] with Child[VTreeImpl]

object VTreeImpl{
  
  def balanced(numVars: Int, offset: Int=0): VTreeImpl = {
    assume(numVars > 0)
    assume(offset >=0)
    if(numVars == 1) return new VTreeLeafImpl(offset+1)
    else return new VTreeINodeImpl(
        balanced(numVars/2,offset),
        balanced(numVars-numVars/2,offset+numVars/2))
  }
  
}

class VTreeLeafImpl(val variable: Variable) extends VTreeImpl with VTreeLeaf[VTreeImpl] {
    
  override val variables = super.variables
  
}

class VTreeINodeImpl(val vl: VTreeImpl, val vr: VTreeImpl) 
  extends VTreeImpl with VTreeINode[VTreeImpl] {

  vl.setParent(this)
  vr.setParent(this)
  
}