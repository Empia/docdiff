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
package matching

abstract class Matcher(f: Double, t: Double) {

  /** Computes the distance between two strings */
  def distance(str1: String, str2: String): Int

  /** Compute a matching between two documents */
  def compute(doc1: TextualConstituent, doc2: TextualConstituent): Set[(TextualConstituent, TextualConstituent)] =
    compute(doc1, doc2, Set(), Set())._1

  /* common matching nodes */
  private def common(matching: Set[(TextualConstituent, TextualConstituent)], n1: TextualConstituent, n2: TextualConstituent): Set[(TextualConstituent, TextualConstituent)] =
    matching.filter {
      case (sn1: LeafConstituent, sn2: LeafConstituent) =>
        n1.contains(sn1) && n2.contains(sn2)
      case _ =>
        false
    }

  private def compute(
    tree1: TextualConstituent,
    tree2: TextualConstituent,
    matched2: Set[TextualConstituent],
    acc: Set[(TextualConstituent, TextualConstituent)]): (Set[(TextualConstituent, TextualConstituent)], Set[TextualConstituent]) = tree1 match {
      case n1 @ LeafConstituent(_, _, _, s1) =>
        val matching = tree2.find {
          case n2 @ LeafConstituent(_, _, _, s2) =>
            !matched2.contains(n2) && (distance(s1, s2).toDouble / math.max(s1.length, s2.length) <= f)
          case _ =>
            false
        }
        matching match {
          case Some(n2) =>
            // we found an unmatched node in tree2 which distance is inferior to the limit
            (acc + (n1 -> n2), matched2 + n2)
          case None =>
            // we did not find any matching node
            (acc, matched2)
        }

      case n1: InternalConstituent =>
        // bottom-up traversal
        val (newAcc, newMatched2) = n1.children.foldLeft((acc, matched2)) {
          case ((acc, matched2), n) =>
            compute(n, tree2, matched2, acc)
        }
        findInternalMatch(n1, tree2, newAcc, newMatched2) match {
          case Some(n2) =>
            // we found an unmatched node in tree2 which distance is inferior to the limit
            (newAcc + (n1 -> n2), newMatched2 + n2)
          case None =>
            // we did not find any matching node
            (newAcc, newMatched2)
        }

    }

    private def findInternalMatch(
      node: InternalConstituent,
      tree2: TextualConstituent,
      acc: Set[(TextualConstituent, TextualConstituent)],
      matched2: Set[TextualConstituent]): Option[TextualConstituent] = node match {
        case n1 @ InternalConstituent(level1, _, name1, _) =>
          tree2.find {
            case n2 @ InternalConstituent(level2, _, name2, _) if level1 == level2 && name1 == name2 =>
              !matched2.contains(n2) && (common(acc, n1, n2).size.toDouble / math.max(n1.size, n2.size) > t)
            case _ =>
              false
          }
      }

}

