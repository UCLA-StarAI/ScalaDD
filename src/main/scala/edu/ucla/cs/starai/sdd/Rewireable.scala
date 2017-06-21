package edu.ucla.cs.starai.sdd

/**
 * An SDD that can be rewired
 */
trait Rewireable[ N <: SDD[N] ] {
  self: N =>
}

trait RewireableElemNode[ N <: SDD[N], U <: RewireableElemNode[N,U] with N ] 
                 extends SDDElemNode[N] with Rewireable[N] {
  
  self: U =>
    
  def rewire(x:N,y:N): U

}

trait RewireableDecNode[ N <: SDD[N], U <: RewireableDecNode[N,U] with N ] 
                 extends SDDDecNode[N] with Rewireable[N] {
  
  self: U =>
    
  def rewire(es:Seq[N]): U
  
}