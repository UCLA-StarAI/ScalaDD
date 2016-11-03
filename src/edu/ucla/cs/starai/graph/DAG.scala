package edu.ucla.cs.starai.graph

import scala.collection.immutable
import scala.collection.mutable

/**
 * A directed acyclic graph with nodes N, leafs L, and internal nodes I
 */
trait DAG[
  +N <: DAG[N,L,I], 
  +L <: LeafNode[N,L,I] with N, 
  +I <: INode[N,L,I] with N
  ] extends Iterable[N] with SelfReferential[N] {
  self: N =>
      
  // TODO: rewrite to not require a full traversal on first next
  override def iterator: Iterator[N] = linearize.iterator
  
  def inodes: Iterator[I] = iterator.collect{case inode:INode[N,L,I] => inode.asSelf}
  def leafs: Iterator[L] = iterator.collect{case leaf:LeafNode[N,L,I] => leaf.asSelf}
  
  def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T = collectUp(_ => true, input, propagate)
  
  
  def collectUp[T](
    select: N => Boolean,
    input: L => T, 
    propagate: (I,Seq[T]) => T): T = {
    val visitedNodes = mutable.HashMap.empty[N, T]
    def visit(node: N): T = {
      visitedNodes.getOrElseUpdate(node, 
        node match {
          case leaf: LeafNode[N,L,I] => input(leaf.asSelf)
          case inode: INode[N,L,I] => {
            val childValues = inode.children.filter(select).map(visit)
            propagate(inode.asSelf,childValues)
          }
        }
      )
    }
    visit(this)
  }
  
    
  override def foreach[U](f: N => U): Unit = {
    foldUp(f, (n:N,x:Any) => f(n))
  }
    
  def selectiveForeach[U](
    select: N => Boolean,
    f: N => U): Unit = {
    collectUp(select, f, (n:N,x:Any) => f(n))
  }
  
  def linearize: Seq[N] = {
    var nodes: List[N] = Nil
    for(node <- self) nodes = node :: nodes
    nodes.reverse
  }
  
  def numNodes: Int = {
    var count = 0;
    for(node <- self) count = count + 1
    count
  }
  
  def numEdges: Int = {
    var count = 0;
    for(node <- self) node match {
      case _:LeafNode[N,L,I] => {}// no edges
      case inode:INode[N,L,I] => count = count + inode.numChildren
    }
    count
  }
  
}

trait LeafNode[+N <: DAG[N,L,I], +L <: LeafNode[N,L,I] with N,+I <: INode[N,L,I] with N] extends DAG[N,L,I]{
  self : L =>
  
  override def iterator: Iterator[N] = Iterator(this)
  
  // no sure why we need this... why doesn't the type system enforce the self type?
  def asSelf = this
  
}
  
trait INode[+N <: DAG[N,L,I], +L <: LeafNode[N,L,I] with N,+I <: INode[N,L,I] with N] extends DAG[N,L,I]{
  self: I =>
  
  // no sure why we need this... why doesn't the type system enforce the self type?
  def asSelf = this
  
  def children: Seq[N]
  
  def numChildren: Int = children.size
  
}