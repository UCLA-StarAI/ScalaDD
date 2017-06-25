package edu.ucla.cs.starai.graph

trait Graph[+N] extends Iterable[N] {
  
  self: N =>
  
  def asSelf: N = this
  
  /**
   * Does this graph contain the given subgraph?
   * Takes any graph in order to covariant in N
   */
  def contains(node: Graph[_]): Boolean = exists { node == _ }
  
  def numNodes: Int = iterator.length
  def numEdges: Int
  
  def linearize: Seq[N] = {
    var nodes: List[N] = Nil
    for(node <- this) nodes = node :: nodes
    nodes.reverse
  }
  
  override def iterator: Iterator[N] = linearize.iterator
  
}