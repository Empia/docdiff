/*
* Copyright (c) 2016 Lucas Satabin
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

package object utils {

  type Interval[V] = (Int, Int, V)

  implicit class IntervalOps[V](val i: Interval[V]) extends AnyVal {

    @inline
    def contains(point: Int): Boolean =
      i._1 <= point && point <= i._2

    @inline
    def overlaps[U](j: Interval[U]): Boolean =
      j._1 <= i._2 && j._2 >= i._1

    @inline
    def overlaps(j: (Int, Int)): Boolean =
      j._1 <= i._2 && j._2 >= i._1

    @inline
    def intersection(j: (Int, Int)): Interval[V] =
      (math.max(i._1, j._1), math.min(i._2, j._2), i._3)

  }

}
