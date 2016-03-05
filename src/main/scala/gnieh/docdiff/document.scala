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

import utils.{
  TrieMap,
  IntervalTree
}

import scala.annotation.tailrec

final case class Document[Metadata, Annotation](metadata: TrieMap[String, Metadata], text: TextualConstituent[Annotation])

sealed abstract class TextualConstituent[Annotation] {
  val level: Int
  val indentation: Int
  val name: String

  def find(p: TextualConstituent[Annotation] => Boolean): Option[TextualConstituent[Annotation]]

  def contains(l: LeafConstituent[Annotation]): Boolean

  def size: Int

}

final case class InternalConstituent[Annotation](level: Int, indentation: Int, name: String, children: Vector[TextualConstituent[Annotation]]) extends TextualConstituent[Annotation] {

  assert(children.forall(child => child.level == this.level - 1 || child.indentation == this.indentation + 1))

  def add(child: TextualConstituent[Annotation]): InternalConstituent[Annotation] = {
    // a child must be either at the direct sub-level or at the direct next indentation
    assert(child.level == this.level - 1 || child.indentation == this.indentation + 1)
    copy(children = children :+ child)
  }

  def find(f: TextualConstituent[Annotation] => Boolean): Option[TextualConstituent[Annotation]] = {
    @tailrec
    def aux(idx: Int): Option[TextualConstituent[Annotation]] =
      if(idx >= children.size)
        None
      else
        children(idx).find(f) match {
          case None => aux(idx + 1)
          case c    => c
        }
    aux(0).orElse(if (f(this)) Some(this) else None)
  }

  def contains(leaf: LeafConstituent[Annotation]) =
    children.exists(_.contains(leaf))

  def size = children.map(_.size).sum

}

final case class LeafConstituent[Annotation](level: Int, indentation: Int, name: String, content: String)(val annotations: IntervalTree[Annotation] = IntervalTree.empty[Annotation]) extends TextualConstituent[Annotation] {

  def find(f: TextualConstituent[Annotation] => Boolean): Option[TextualConstituent[Annotation]] =
    if (f(this))
      Some(this)
    else
      None

  def contains(leaf: LeafConstituent[Annotation]) =
    leaf.level == this.level && leaf.name == this.name && leaf.content == this.content

  val size = 1

}
