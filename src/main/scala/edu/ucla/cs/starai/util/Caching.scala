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

import scala.collection.mutable

  
/**
 * Mixin for classes that want to provide cache arguments instead of lazy vals.
 * Avoids boilerplate related to typing the self class in the map.
 */
trait Caching[T]{
 
 self: T =>
  
 type Cache[O] = mutable.Map[T,O]
 type Cache1[I,O] = mutable.Map[(T,I),O]
 
 def emptyCache[O]: Cache[O] = mutable.Map.empty[T,O]
 def emptyCache1[I,O]: Cache1[I,O] = mutable.Map.empty[(T,I),O]
 
}