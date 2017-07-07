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
 * An object whose parent can be set
 */
trait Child[P]{
  
  private var _parent: Option[P] = None
  
  def setParent(p: P){
    require(p != null)
    require(_parent.isEmpty, s"$this cannot have multiple parents ($parent and $p)")
    _parent = Some(p)
  }
  
  def parent: Option[P] = _parent
  
}
