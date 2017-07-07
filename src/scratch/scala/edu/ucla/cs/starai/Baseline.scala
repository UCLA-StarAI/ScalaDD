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

import scala.io.Source
import java.io.File
import sys.process._

object Baseline extends App {
  
  val suite = "easy"
  
  val cnfsPath = getClass.getResource("/cnfs").getPath
  println("CNFs: "+cnfsPath)
  
  val cnfsDir = new File(cnfsPath)  
  println("Suits: "+cnfsDir.listFiles.map(_.getName).mkString(", "))
  
  val suiteDir = new File(cnfsDir,s"$suite/")
  require(suiteDir.exists(), s"Suite $suite does not exist.")
  val files = suiteDir.listFiles
  require(files != null)
  
  for(file <- files.filter(_.toString.endsWith(".cnf")).sortBy(_.length())){
    println(s"Processing file ${file.getName} in $suite")
    val base: String = file.toString.replaceAll("\\.[^.]*$", "")
    minimized(base)
    balanced(base)
    println 
  }
  
  def balanced(base: String){
    val vtree = base+".balanced.vtree"
    val log = base+".balanced.log"
    println(s"Saving balanced vtree in ${vtree}")
    val cmd = s"sdd -c $base.cnf -W $vtree -r 0 -t balanced -M"
    println("$ " + cmd)
    val result = (cmd #>> new File(log)).!
    require(result==0, s"Failed command: $cmd")
  }
  
  def minimized(base: String){
    val vtree = base+".min.vtree"
    val log = base+".min.log"
    println(s"Saving minimized vtree in ${vtree}")
    val cmd = s"sdd -c $base.cnf -W $vtree -r 2"
    println("$ " + cmd)
    val result = (cmd #>> new File(log)).!
    require(result==0, s"Failed command: $cmd")
  }
  
}