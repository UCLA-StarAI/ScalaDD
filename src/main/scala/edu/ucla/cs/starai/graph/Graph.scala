package edu.ucla.cs.starai.graph

trait Graph[+N] extends Iterable[N] {
      
  /**
   * Does this graph contain the given subgraph?
   * Takes any graph in order to covariant in N
   */
  def contains[M >: N](node: M): Boolean = exists { node == _ }
  
  def numNodes: Int = iterator.length
  def numEdges: Int
  
  def linearize: Seq[N] = {
    var nodes: List[N] = Nil
    for(node <- this) nodes = node :: nodes
    nodes.reverse
  }
  
  override def iterator: Iterator[N] = linearize.iterator
  
}