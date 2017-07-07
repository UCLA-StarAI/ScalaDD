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

package edu.ucla.cs.starai.util

import scala.language.implicitConversions

object Binomial {

  private[this] val factorialCache = new collection.mutable.ArrayBuffer[LogDouble] ++ List(LogDouble.one, LogDouble.one)

  def factorial(n: Int): LogDouble = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): LogDouble = factorial(n) / factorial(k) / factorial(n - k)

}