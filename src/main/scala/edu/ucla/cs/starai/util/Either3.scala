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

/**
 * Extends Either to three elements
 */
sealed trait Either3[+L,+M,+R]

case class Left3[+L,+M,+R](l: L) extends Either3[L,M,R]
case class Mid3[+L,+M,+R](m: M) extends Either3[L,M,R]
case class Right3[+L,+M,+R](r: R) extends Either3[L,M,R]