package edu.ucla.cs.starai.util

/**
 * An object whose parent can be set
 */
trait Child[P]{
  
  private var _parent: Option[P] = None
  
  def setParent(p: P){
    require(p != null)
    require(_parent.isEmpty, s"$this cannot have multiple parents ($parent and $p)")
    _parent = Some(p)
  }
  
  def parent: Option[P] = _parent
  
}
