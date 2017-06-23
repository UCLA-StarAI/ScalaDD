package edu.ucla.cs.starai.graph

trait Graph[+N] extends Iterable[N] {
  
  def contains[U>:N](node: U): Boolean = exists { node == _ }
  
  def numNodes: Int = iterator.length
  def numEdges: Int
  
  def linearize: Seq[N] = {
    var nodes: List[N] = Nil
    for(node <- this) nodes = node :: nodes
    nodes.reverse
  }
  
  // TODO: rewrite to not require a full traversal on first next
  override def iterator: Iterator[N] = linearize.iterator
}