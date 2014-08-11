/*
* This file is part of the docdiff project.
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

sealed trait Node extends Ordered[Node] {

  protected[docdiff] var parent: Option[Node]

  /** Indicates whether this node contains this sentence */
  def contains(sentence: Sentence): Boolean

  /** Finds the first node (bottom-up looking) satisfying the predicate */
  def find(f: Node => Boolean): Option[Node]

  /** Returns the number of leaves for this node */
  def size: Int

  /** Returns the ordered list of node of type `lbl` in the same order they appear in the document */
  def chain(lbl: Node): List[Node]

}

sealed abstract class InternalNode extends Node {
  val children: List[Node]

  children.foreach(_.parent = Some(this))

  def find(f: Node => Boolean): Option[Node] = {
    def aux(children: List[Node]): Option[Node] = children match {
      case child :: tail =>
        child.find(f).orElse(aux(tail))
      case Nil =>
        None
    }
    aux(children).orElse(if (f(this)) Some(this) else None)
  }

  def size = children.map(_.size).sum

}

final case class Document(children: List[Node]) extends InternalNode {
  protected[docdiff] var parent: Option[Node] = None
  def compare(that: Node) = that match {
    case Document(_) => 0
    case _           => 1
  }

  def contains(sentence: Sentence) =
    children.exists(_.contains(sentence))

  def chain(lbl: Node): List[Node] =
    lbl match {
      case Document(_) =>
        this :: children.flatMap(_.chain(lbl))
      case _ if lbl <= this =>
        children.flatMap(_.chain(lbl))
      case _ =>
        Nil
    }

}

final case class Title(level: Int, name: Sentence, children: List[Node]) extends InternalNode {

  protected[docdiff] var parent: Option[Node] = None

  def compare(that: Node) = that match {
    case Title(thatLevel, _, _) => thatLevel - this.level
    case Document(_)            => -1
    case _                      => 1
  }

  def contains(sentence: Sentence) =
    name == sentence || children.exists(_.contains(sentence))

  def chain(lbl: Node): List[Node] =
    lbl match {
      case Title(lvl, _, _) if lvl == level =>
        this :: children.flatMap(_.chain(lbl))
      case _ if lbl <= this =>
        children.flatMap(_.chain(lbl))
      case _ =>
        Nil
    }

}

final case class Paragraph(children: List[Node]) extends InternalNode {

  protected[docdiff] var parent: Option[Node] = None

  def compare(that: Node) = that match {
    case Paragraph(_)                   => 0
    case Sentence(_) | Enumerated(_, _) => 1
    case _                              => -1
  }

  def contains(sentence: Sentence) =
    children.exists(_.contains(sentence))

  def chain(lbl: Node): List[Node] =
    lbl match {
      case Paragraph(_) =>
        this :: children.flatMap(_.chain(lbl))
      case _ if lbl <= this =>
        children.flatMap(_.chain(lbl))
      case _ =>
        Nil
    }

}

final case class Enumerated(numbered: Boolean, children: List[Node]) extends InternalNode {

  protected[docdiff] var parent: Option[Node] = None

  def compare(that: Node) = that match {
    case Enumerated(_, _) => 0
    case Sentence(_)      => 1
    case _                => -1
  }

  def contains(sentence: Sentence) =
    children.exists(_.contains(sentence))

  def chain(lbl: Node): List[Node] =
    lbl match {
      case Enumerated(_, _) =>
        this :: children.flatMap(_.chain(lbl))
      case _ if lbl <= this =>
        children.flatMap(_.chain(lbl))
      case _ =>
        Nil
    }

}

final case class Sentence(content: String) extends Node {

  protected[docdiff] var parent: Option[Node] = None

  def compare(that: Node) = that match {
    case Sentence(_) => 0
    case _           => -1
  }

  def contains(sentence: Sentence) =
    sentence == this

  def find(f: Node => Boolean): Option[Node] =
    if (f(this))
      Some(this)
    else
      None

  def size = 1

  def chain(lbl: Node): List[Node] = lbl match {
    case Sentence(_) => List(this)
    case _           => Nil
  }

}

