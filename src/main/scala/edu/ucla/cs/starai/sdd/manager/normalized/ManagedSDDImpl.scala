package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic._
import edu.ucla.cs.starai.util.Child
import edu.ucla.cs.starai.sdd.manager.GoogleWeakCache

abstract class SDDManagerImpl extends SDDManager with Child[SDDManagerImpl]

object SDDManagerImpl{
  
  def apply(vtree: VTree.SomeVtree): SDDManagerImpl = vtree.kind match{
    case Left(leaf) => new ManagedSDDLeafImpl(leaf.variable)
    case Right(inode) => new ManagedSDDINodeImpl(
      SDDManagerImpl(inode.vl), SDDManagerImpl(inode.vr)
    )
  }
  
}

class ManagedSDDLeafImpl(val variable: Variable) 
  extends SDDManagerImpl with SDDManagerLeaf{
    
  override val variables = super.variables
  
}

class ManagedSDDINodeImpl(val vl: SDDManagerImpl, val vr: SDDManagerImpl) 
  extends SDDManagerImpl with SDDManagerINode {

  val uniqueNodesCache = new GoogleWeakCache
  
  vl.setParent(this)
  vr.setParent(this)
  
}