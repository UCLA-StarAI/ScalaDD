package edu.ucla.cs.starai.graph

import scala.collection.immutable
import scala.collection.mutable
import edu.ucla.cs.starai.util.Caching

/**
 * A singly-connected DAG, with nodes N
 */
trait Tree[+N <: Tree[N]] extends DAG[N] {
  
  self: N =>  
  
  override def foldUp[T](
    propagate: (N,Seq[T]) => T): T = {
      propagate(self,children.map(_.foldUp(propagate)))
  } 
  
  override def iterator: Iterator[N] = children.map(_.iterator).reduce(_ ++ _) ++ Iterator(this)
  
}


trait DoubleLinkedTree[+N <: DoubleLinkedTree[N]] extends Tree[N] {
    
  self: N =>
    
    def parent: Option[N]
    
    def ancestors: List[N] = parent.map(p => p :: p.ancestors).getOrElse(Nil)
    
    def root = ancestors.last
    
    // prefer exception over option return type here
    def lca[U >: N <: DoubleLinkedTree[U]](that: U): U = {
      if(this == that) this
      else if(this.contains(that)) this
      else if (that.contains(this)) that.asSelf
      else {
        val thisParents = this.ancestors.toSet[U]
        for(parent <- that.ancestors)
          if(thisParents.contains(parent)) 
            return parent
        throw new IllegalArgumentException(s"$this and $that share no common ancestor")
      }
    }
    
    /**
     * Returns first ancestor (including this) that satisfies a predicate
     */
    // prefer exception over option return type here
    def ancestor(pred: (N => Boolean)): N = {
      if(pred(this)) this
      else if(parent.nonEmpty) parent.get.ancestor(pred)
      else throw new IllegalArgumentException(s"$this does not have an ancestor that satisfies the given predicate")
    }
    
    
}