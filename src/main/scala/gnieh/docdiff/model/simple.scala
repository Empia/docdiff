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
package model

class Simple[Annotation](val headings: Int) {

  object Body {
    def apply(content: Vector[TextualConstituent[Annotation]]): InternalConstituent[Annotation] =
      InternalConstituent(headings + 2, 0, "body", content)
  }

  object Heading {
    def unapply(c: TextualConstituent[Annotation]): Option[Int] =
      c match {
        case InternalConstituent(level, _, "title", _) if level >= 2 && level < headings + 1 =>
          Some(level)
        case _ =>
          None
      }
  }

  object Paragraph {
    def apply(text: Vector[TextualConstituent[Annotation]])(implicit indent: Int): InternalConstituent[Annotation] =
      InternalConstituent(1, indent, "paragraph", text)

    def unapply(c: TextualConstituent[Annotation]): Option[Vector[TextualConstituent[Annotation]]] =
      c match {
        case InternalConstituent(_, _, "paragraph", children) =>
          Some(children)
        case _ =>
          None
      }
  }

  object Sentence {
    def apply(text: String)(implicit indent: Int): LeafConstituent[Annotation] =
      LeafConstituent(0, indent, "sentence", text)()

    def unapply(c: TextualConstituent[Annotation]): Option[String] =
      c match {
        case LeafConstituent(_, _, "sentence", text) =>
          Some(text)
        case _ =>
          None
      }
  }

}
