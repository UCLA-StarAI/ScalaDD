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

package edu.ucla.cs.starai.util

import scala.language.implicitConversions

object KLD {

  import SignLogDouble._
  
  def asymmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    if (p.isZero) {
      zero
    } else if (q.isZero) {
    	// smooth
        asymmetricKld(p, 0.000001)
    } else {
    	p * (p / q).log
    }
  }

  def symmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    val r1 = asymmetricKld(p, q)
    val r2 = asymmetricKld(q, p)
    (r1 + r2)
  }
  
  def symmetricKld(p: SignLogDouble, q: SignLogDouble, r: SignLogDouble): SignLogDouble = {
    val r1 = symmetricKld(p, q)
    val r2 = symmetricKld(q, r)
    val r3 = symmetricKld(p, r)
    (r1 + r2 + r3)
  }

}