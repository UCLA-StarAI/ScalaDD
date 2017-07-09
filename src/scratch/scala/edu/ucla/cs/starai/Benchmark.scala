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
import edu.ucla.cs.starai.util._
import edu.ucla.cs.starai.sdd.compiler.TreeCompiler
import edu.ucla.cs.starai.logic.io.VTreeParser
import edu.ucla.cs.starai.logic.DimacsIO

object BenchMark extends App {
  
  val suite = "iscas89"
//  val suite = "easy"
  val vtreeType = "min"
  
  val vtreeParser = new VTreeParser(0)
  val sddCompiler = new TreeCompiler(0)
  
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
    if((new File(s"$base.$vtreeType.vtree")).exists()){
      benchmark(base)
    }else println(s"No vtree available named '$base.$vtreeType.vtree'")
    println 
  }
  
  def benchmark(base: String){
    //println(s"Parsing $base.cnf")
    val cnf = DimacsIO.parse(Source.fromFile(s"$base.cnf"))
    //println(s"Parsing $base.$vtreeType.vtree")
    val vtree = vtreeParser.parse(Source.fromFile(s"$base.$vtreeType.vtree"))
    val sdd = time("Compilation"){sddCompiler.compile(cnf, vtree)}
  }
  
}