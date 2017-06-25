package edu.ucla.cs.starai

package object logic {
  
  implicit class Variable(val under: Int) extends AnyVal {
    
    def unary_! : Literal = new Literal(-under)
    def unary_- = unary_!
    
    def toLiteral: Literal = under
    def toInt = under
        
    override def toString = s"X$under"
    
  }
  
  implicit class Literal(val under: Int) extends AnyVal {
       
    def unary_! : Literal = new Literal(-under)
    def unary_- = unary_!
    
    def variable = new Variable(under.abs)
    def toInt = under
    
    def isPositive = (under > 0)
    def isNegative = (under < 0)
    
    override def toString = if(isPositive) variable.toString else s"-$variable" 
  }
  
  implicit def variable2Literal(v: Variable): Literal = v.toLiteral
    
}