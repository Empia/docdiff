package gnieh.docdiff
package matching

abstract class Matcher(f: Double, t: Double) {

  /** Computes the distance between two strings */
  def distance(str1: String, str2: String): Int

  /** Compute a matching between two documents */
  def compute(doc1: Document, doc2: Document): Set[(Node, Node)] =
    compute(doc1, doc2, Set(), Set())._1

  /* common matching nodes */
  private def common(matching: Set[(Node, Node)], n1: Node, n2: Node): Set[(Node, Node)] =
    matching.filter {
      case (sn1: Sentence, sn2: Sentence) =>
        n1.contains(sn1) && n2.contains(sn2)
      case _ =>
        false
    }

  private def compute(
    tree1: Node,
    tree2: Node,
    matched2: Set[Node],
    acc: Set[(Node, Node)]): (Set[(Node, Node)], Set[Node]) = tree1 match {
      case n1 @ Sentence(s1) =>
        val matching = tree2.find {
          case n2 @ Sentence(s2) =>
            !matched2.contains(n2) && distance(s1, s2) <= f
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

      case n1: InternalNode =>
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
      node: InternalNode,
      tree2: Node,
      acc: Set[(Node, Node)],
      matched2: Set[Node]): Option[Node] = node match {
        case n1 @ Paragraph(_) =>
          tree2.find {
            case n2 @ Paragraph(_) =>
              !matched2.contains(n2) && (common(acc, n1, n2).size.toDouble / math.max(n1.size, n2.size) > t)
            case _ =>
              false
          }

        case n1 @ Enumerated(numbered1, _) =>
          tree2.find {
            case n2 @ Enumerated(numbered2, _) =>
              !matched2.contains(n2) && numbered1 == numbered2 && (common(acc, n1, n2).size.toDouble / math.max(n1.size, n2.size) > t)
            case _ =>
              false
          }

        case n1 @ Title(level1, title1, _) =>
          tree2.find {
            case n2 @ Title(level2, title2, _) =>
              !matched2.contains(n2) && level1 == level2 && (common(acc, n1, n2).size.toDouble / math.max(n1.size, n2.size) > t)
            case _ =>
              false
          }

        case n1 @ Document(_) =>
          tree2.find {
            case n2 @ Document(_) =>
              !matched2.contains(n2) && (common(acc, n1, n2).size.toDouble / math.max(n1.size, n2.size) > t)
            case _ =>
              false
          }
      }

}

