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

import utils.TrieMap

import scala.annotation.tailrec

final case class Document(metadata: TrieMap[String, Metadata], text: TextualConstituent)

sealed abstract class TextualConstituent {
  val level: Int
  val indentation: Int
  val name: String

  def find(p: TextualConstituent => Boolean): Option[TextualConstituent]

  def contains(l: LeafConstituent): Boolean

  def size: Int

}

final case class InternalConstituent(level: Int, indentation: Int, name: String, children: Vector[TextualConstituent]) extends TextualConstituent {

  def add(child: TextualConstituent): InternalConstituent = {
    // a child must be either at the direct sub-level or at the direct next indentation
    assert(child.level == this.level - 1 || child.indentation == this.indentation + 1)
    copy(children = children :+ child)
  }

  def find(f: TextualConstituent => Boolean): Option[TextualConstituent] = {
    @tailrec
    def aux(idx: Int): Option[TextualConstituent] =
      if(idx >= children.size)
        None
      else
        children(idx).find(f) match {
          case None => aux(idx + 1)
          case c    => c
        }
    aux(0).orElse(if (f(this)) Some(this) else None)
  }

  def contains(leaf: LeafConstituent) =
    children.exists(_.contains(leaf))

  def size = children.map(_.size).sum

}

final case class LeafConstituent(level: Int, indentation: Int, name: String, content: String) extends TextualConstituent {

  def find(f: TextualConstituent => Boolean): Option[TextualConstituent] =
    if (f(this))
      Some(this)
    else
      None

  def contains(leaf: LeafConstituent) =
    leaf == this

  val size = 1

}

trait Metadata
