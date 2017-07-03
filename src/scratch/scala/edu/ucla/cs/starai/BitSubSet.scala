/*
 * Copyright 2017 Guy Van den Broeck
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

package edu.ucla.cs.starai;

import edu.ucla.cs.starai.util.BitSubSet

object BitSubSetScratch extends App {
  
  val s = BitSubSet(Array("a", "c", "d"))
  println(s)
  val t = s - "c"
  println(t)
  val u = t + "c"
  println(u)
  val v = u.filter(_ != "d")
  println(v)
  println(v.size)
  val w = v intersect t
  println(w)
  val x = v union t
  println(x)
  
}

