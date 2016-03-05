/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh.docdiff
package utils

class TrieMap[K, V] private(value: Option[V], children: Map[K, TrieMap[K, V]]) {

  def this() =
    this(None, Map.empty)

  def get(path: List[K]): Option[V] =
    path match {
      case Nil    => value
      case h :: t => children.get(h).flatMap(_.get(t))
    }

  def updated(path: List[K], value: V): TrieMap[K, V] =
    path match {
      case Nil    => new TrieMap(Some(value), children)
      case h :: t => new TrieMap(this.value, children.updated(h, children.getOrElse(h, new TrieMap[K, V]).updated(t, value)))
    }

  def remove(path: List[K]): TrieMap[K, V] =
    path match {
      case Nil     => new TrieMap(None, children)
      case List(k) => new TrieMap(value, children - k)
      case h :: t  =>
        children.get(h) match {
          case Some(c) => new TrieMap(value, children.updated(h, c.remove(t)))
          case None    => this
        }
    }

}
