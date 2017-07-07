/*
 * Copyright 2017 Guy Van den Broeck <guyvdb@cs.ucla.edu>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
  
  override def iterator: Iterator[N] = 
    children.map(_.iterator).foldRight(Iterator(this))(_ ++ _)
  
}


trait DoubleLinkedTree[+N <: DoubleLinkedTree[N]] extends Tree[N] {
    
  self: N =>
    
    def parent: Option[N]
    
    def ancestors: List[N] = parent.map(p => p :: p.ancestors).getOrElse(Nil)
    
    def root = (this :: ancestors).last
    
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