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

package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util.Child
import edu.ucla.cs.starai.sdd.manager.GoogleWeakCache
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache

abstract class SDDManagerImpl extends SDDManager with Child[SDDManagerImpl]{
  
}

object SDDManagerImpl{
  
  def apply(vtree: VTree.Some): SDDManagerImpl = vtree.kind match{
    case Left(leaf) => new SDDManagerLeafImpl(leaf.variable)
    case Right(inode) => new SDDManagerINodeImpl(
      SDDManagerImpl(inode.vl), SDDManagerImpl(inode.vr)
    )
  }
  
}

class SDDManagerLeafImpl(_variable: Variable) 
  extends { val variable = _variable } 
  with SDDManagerImpl with SDDManagerLeaf{
  
}

class SDDManagerINodeImpl(val vl: SDDManagerImpl, val vr: SDDManagerImpl) extends {
  
    val uniqueNodesCache = new GoogleWeakCache[ManagedSDD]
    
  } with SDDManagerImpl with SDDManagerINode {
  
  vl.setParent(this)
  vr.setParent(this)
  
}