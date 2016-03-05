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
package utils

import scala.collection.immutable.VectorBuilder

object  IntervalTree {

  def empty[V]: IntervalTree[V] =
    new IntervalTree(None)

  def fromIntervals[V](intervals: Seq[Interval[V]]): IntervalTree[V] =
    new IntervalTree(divideIntervals(intervals))

  private def divideIntervals[V](intervals: Seq[Interval[V]]): Option[Node[V]] =
    if(intervals.isEmpty) {
      None
    } else {
      val xCenter = center(intervals)

      val sCenter = new VectorBuilder[Interval[V]]
      val sLeft = new VectorBuilder[Interval[V]]
      val sRight = new VectorBuilder[Interval[V]]

      for(i <- intervals) {
        if(i._2 < xCenter)
          sLeft += i
        else if(xCenter < i._1)
          sRight += i
        else
          sCenter += i
      }

      Some(Node(xCenter, sCenter.result.sortBy(_._1), divideIntervals(sLeft.result), divideIntervals(sRight.result)))

    }

    private def center[V](intervals: Seq[Interval[V]]): Int = {
      val fs = intervals.sortBy(_._1)
      fs(fs.size / 2)._1
    }

}

// assumes sCenter is sorted by lower bounds
private case class Node[V](xCenter: Int, sCenter: Vector[Interval[V]], left: Option[Node[V]], right: Option[Node[V]]) {

  def search(point: Int, result: VectorBuilder[V]): Unit = {
    for {
      i <- sCenter
      if i.contains(point)
    } result += i._3
    if(point <= xCenter)
      for(n <- left)
        n.search(point, result)
    if(point > xCenter)
      for(n <- right)
        n.search(point, result)
  }

  def collect[U <: V](interval: (Int, Int), result: VectorBuilder[Interval[U]], pf: PartialFunction[V, U]): Unit = {
    val (first, last) = interval
    for {
      i <- sCenter
      if i.overlaps(interval) && pf.isDefinedAt(i._3)
      (lo, hi, v) = i.intersection(interval)
    } result += ((lo, hi, pf(v)))
    if(first < xCenter)
      for(n <- left)
        n.collect(interval, result, pf)
    if(last > xCenter)
      for(n <- right)
        n.collect(interval, result, pf)
  }

}

class IntervalTree[V] private (root: Option[Node[V]]) {

  /** Collects tha values within the input interval for which the partial function can be applied. */
  def collect[U <: V](interval: (Int, Int))(pf: PartialFunction[V, U]): Set[Interval[U]] = root match {
    case Some(root) =>
      val result = new VectorBuilder[Interval[U]]
      root.collect(interval, result, pf)
      result.mapResult(_.toSet[Interval[U]]).result
    case None =>
      Set.empty[Interval[U]]
  }

  /** Searches all the values applying at the given point. */
  def search(point: Int): Set[V] = root match {
    case Some(root) =>
      val result = new VectorBuilder[V]
      root.search(point, result)
      result.mapResult(_.toSet[V]).result
    case None =>
      Set.empty[V]
  }

}
