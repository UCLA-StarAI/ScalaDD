package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util.Child
import edu.ucla.cs.starai.sdd.manager.GoogleWeakCache
import edu.ucla.cs.starai.sdd.manager.UniqueNodesCache

abstract class SDDManagerImpl extends SDDManager with Child[SDDManagerImpl]

object SDDManagerImpl{
  
  def apply(vtree: VTree.SomeVtree): SDDManagerImpl = vtree.kind match{
    case Left(leaf) => new ManagedSDDLeafImpl(leaf.variable)
    case Right(inode) => new ManagedSDDINodeImpl(
      SDDManagerImpl(inode.vl), SDDManagerImpl(inode.vr)
    )
  }
  
}

class ManagedSDDLeafImpl(_variable: Variable) 
  extends { val variable = _variable } 
  with SDDManagerImpl with SDDManagerLeaf{
  
}

class ManagedSDDINodeImpl(val vl: SDDManagerImpl, val vr: SDDManagerImpl) extends {
    val uniqueNodesCache: UniqueNodesCache[ManagedSDD] = new GoogleWeakCache
  } with SDDManagerImpl with SDDManagerINode {
  
  vl.setParent(this)
  vr.setParent(this)
  
}