package edu.ucla.cs.starai.graph

import scala.collection.immutable
import scala.collection.mutable

/**
 * A singly-connected DAG, with nodes N, leafs L, and internal nodes I
 */
trait Tree[
  +N <: Tree[N,L,I], 
  +L <: TreeLeaf[N,L,I] with N, 
  +I <: TreeINode[N,L,I] with N
  ] extends DAG[N,L,I]  {
  
  self: N =>  
  
  // opportunity to specialize methods
       
}


trait TreeLeaf[+N <: Tree[N,L,I], +L <: TreeLeaf[N,L,I] with N,+I <: TreeINode[N,L,I] with N] 
  extends Tree[N,L,I] with LeafNode[N,L,I]{
  
  self: L =>
    
}
  

trait TreeINode[+N <: Tree[N,L,I], +L <: TreeLeaf[N,L,I] with N,+I <: TreeINode[N,L,I] with N] 
  extends Tree[N,L,I] with INode[N,L,I]{
  
  self: I =>
  
  
  def children: Seq[N]
   
  override def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T = {
      propagate(self,children.map(_.foldUp(input,propagate)))
  } 
  
  override def iterator: Iterator[N] = children.map(_.iterator).reduce(_ ++ _) ++ Iterator(this)
  
}