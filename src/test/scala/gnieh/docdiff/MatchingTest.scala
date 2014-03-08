package gnieh.docdiff
package matching

import org.scalatest._

class MatchingTest extends FunSuite with ShouldMatchers {

  val a1 = Sentence("a")
  val b1 = Sentence("b")
  val c1 = Sentence("c")
  val d1 = Sentence("d")
  val e1 = Sentence("e")
  val f1 = Sentence("f")

  val p11 = Paragraph(List(a1, b1, c1))
  val p21 = Paragraph(List(d1, e1))
  val p31 = Paragraph(List(f1))

  val a2 = Sentence("a")
  val c2 = Sentence("c")
  val d2 = Sentence("d")
  val e2 = Sentence("e")
  val f2 = Sentence("f")
  val g2 = Sentence("g")

  val p12 = Paragraph(List(a2, c2))
  val p22 = Paragraph(List(f2))
  val p32 = Paragraph(List(d2, e2, g2))

  val doc1 = Document(List(p11, p21, p31))

  val doc2 = Document(List(p12, p22, p32))

  test("compute matching") {
    val expected = Set(
      (a1, a1.parent, a2, a2.parent),
      (c1, c1.parent, c2, c2.parent),
      (d1, d1.parent, d2, d2.parent),
      (e1, e1.parent, e2, e2.parent),
      (f1, f1.parent, f2, f2.parent),
      (p11, p11.parent, p12, p12.parent),
      (p21, p21.parent, p32, p32.parent),
      (p31, p31.parent, p22, p22.parent),
      (doc1, doc1.parent, doc2, doc2.parent)
    )
    val matcher = new LevenshteinMatcher(0.5, 0.5)

    val output = matcher.compute(doc1, doc2)

    output.map { case (n1, n2) => (n1, n1.parent, n2, n2.parent) } should be(expected)

  }

}

