package edu.ucla.cs.starai.sdd.manager.normalized

import edu.ucla.cs.starai.logic.VTreeLeafImpl
import edu.ucla.cs.starai.logic._

trait SDDManagerImpl extends ChildSDDManager

object SDDManagerImpl{
  
  def apply(vtree: VTree[_]): ChildSDDManager = vtree.kind match{
    case Left(leaf) => new ManagedSDDLeafImpl(leaf.variable)
    case Right(inode) => new ManagedSDDINodeImpl(
      SDDManagerImpl(inode.vl), SDDManagerImpl(inode.vr)
    )
  }
  
}

class ManagedSDDLeafImpl(variable: Variable) 
  extends VTreeLeafImpl(variable: Variable) with SDDManagerLeaf with SDDManagerImpl
  
class ManagedSDDINodeImpl(vl: ChildSDDManager, vr: ChildSDDManager) 
  extends VTreeINodeImpl(vl, vr) with SDDManagerINode with SDDManagerImpl
  