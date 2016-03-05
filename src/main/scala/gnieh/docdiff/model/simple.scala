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
package model

class Simple(val headings: Int) {

  object Body {
    def apply(content: Vector[TextualConstituent]): InternalConstituent =
      InternalConstituent(0, 0, "body", content)
  }

  object Heading {
    def unapply(c: TextualConstituent): Option[Int] =
      c match {
        case InternalConstituent(level, _, "title", _) if level < headings =>
          Some(level)
        case _ =>
          None
      }
  }

  object Paragraph {
    def apply(text: Vector[TextualConstituent])(implicit indent: Int): InternalConstituent =
      InternalConstituent(headings + 2, indent, "paragraph", text)

    def unapply(c: TextualConstituent): Option[Vector[TextualConstituent]] =
      c match {
        case InternalConstituent(_, _, "paragraph", children) =>
          Some(children)
        case _ =>
          None
      }
  }

  object Sentence {
    def apply(text: String)(implicit indent: Int): LeafConstituent =
      LeafConstituent(headings + 3, indent, "sentence", text)

    def unapply(c: TextualConstituent): Option[String] =
      c match {
        case LeafConstituent(_, _, "sentence", text) =>
          Some(text)
        case _ =>
          None
      }
  }

}
