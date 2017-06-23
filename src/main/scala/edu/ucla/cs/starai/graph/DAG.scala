package edu.ucla.cs.starai.graph

import scala.collection.immutable
import scala.collection.mutable

/**
 * A directed acyclic graph with nodes N and leafs L.
 */
trait DAG[
  +N <: DAG[N,L,I],
  +L <: LeafNode[N,L,I] with N,
  +I <: INode[N,L,I] with N
  ] extends Graph[N] with SelfTraversable[N] {
  
  // self type enforces that object's subtype of DAG is also the subtype of DAG passed as N
  // self: N =>
  
  def leafs: Iterator[L] = iterator.collect{case leaf:LeafNode[N,L,I] => 
    leaf.asInstanceOf[L] // Self type of LeafNode[N,L,I] enforces that it is an instance of L.
  }
  
  def inodes: Iterator[I] = iterator.collect{case inode:INode[N,L,I] => 
    inode.asInstanceOf[I] // Self type of INode[N,L,I] enforces that it is an instance of I.
  }
  
  def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T
    
  def foldUpCached[T,M>:N](
    input: L => T, 
    propagate: (I,Seq[T]) => T,
    cache: mutable.Map[M, T]): T
  
  override def foreach[U](f: N => U): Unit = {
    foldUp(f,(x,_:Any) => f(x))
  }
    
  override def numEdges: Int = {
    var count = 0;
    for(inode <- inodes) count = count + inode.numChildren
    count
  }
  
}

trait LeafNode[
  +N <: DAG[N,L,I],
  +L <: LeafNode[N,L,I] with N,
  +I <: INode[N,L,I] with N
  ] extends DAG[N,L,I] {
  
  // self type enforces that object's subtype of LeafNode is also the subtype of LeafNode passed as L
  self: L =>
    
  override def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T  = input(this)
    
  override def foldUpCached[T,M>:N](
    input: L => T, 
    propagate: (I,Seq[T]) => T,
    cache: mutable.Map[M, T]): T = {
    cache.getOrElseUpdate(this, input(this))
  }
  
  override def foreach[U](f: N => U): Unit = f(this)
  
  override def numNodes = 1
  override def numEdges = 0
  
  override def iterator: Iterator[L] = Iterator(this)
  
}
  
trait INode[
  +N <: DAG[N,L,I],
  +L <: LeafNode[N,L,I] with N,
  +I <: INode[N,L,I] with N
  ] extends DAG[N,L,I]{
    
  // self type enforces that object's subtype of INode is also the subtype of INode passed as I
  self: I =>
    
  override def foldUp[T](
    input: L => T, 
    propagate: (I,Seq[T]) => T): T  = {
    foldUpCached(input,propagate,mutable.Map.empty)
  }
    
  override def foldUpCached[T,M>:N](
    input: L => T, 
    propagate: (I,Seq[T]) => T,
    cache: mutable.Map[M, T]): T = {
    cache.getOrElseUpdate(this, 
        propagate(this,children.map(_.foldUpCached(input,propagate,cache))))
  }
  
  def children: Seq[N]
  
  def numChildren: Int = children.size
  
}