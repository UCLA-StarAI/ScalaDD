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

package edu.ucla.cs.starai

import scala.language.implicitConversions

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
    
    override def toString = if(isPositive) variable.toString else "-"+variable.toString
  }
  
  implicit def variable2Literal(v: Variable): Literal = v.toLiteral
  
}