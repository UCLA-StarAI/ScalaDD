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
  ] extends Iterable[N] with SelfTraversable[N] {
  self: N =>
      
  // TODO: rewrite to not require a full traversal on first next
  override def iterator: Iterator[N] = linearize.iterator
  
  def inodes: Iterator[I] = iterator.collect{case inode:INode[N,L,I] => inode.asSelfINode}
  def leafs: Iterator[L] = iterator.collect{case leaf:LeafNode[N,L,I] => leaf.asSelfLeaf}
  
  def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T = {
    val visitedNodes = mutable.HashMap.empty[N, T]
    def visit(node: N): T = {
      visitedNodes.getOrElseUpdate(node, 
        node match {
          case leaf: LeafNode[N,L,I] => input(leaf.asSelfLeaf)
          case inode: INode[N,L,I] => {
            val childValues = inode.children.map(visit)
            propagate(inode.asSelfINode,childValues)
          }
        }
      )
    }
    visit(this)
  }
    
  override def foreach[U](f: N => U): Unit = {
    foldUp(f, (n:N,x:Any) => f(n))
  }
  
  def contains[U>:N](node: U): Boolean = exists { node == _ }
      
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
  
  // not sure why we need this... why doesn't the type system enforce the self type?
  def asSelfLeaf = this
  
}
  
trait INode[+N <: DAG[N,L,I], +L <: LeafNode[N,L,I] with N,+I <: INode[N,L,I] with N] extends DAG[N,L,I]{
  self: I =>
  
  // not sure why we need this... why doesn't the type system enforce the self type?
  def asSelfINode = this
  
  def children: Seq[N]
  
  def numChildren: Int = children.size
  
}