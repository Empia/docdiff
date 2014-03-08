package gnieh.docdiff
package matching

/** A document matcher that uses Levenshtein distance to compare
 *  two sentences.
 *
 *  @author Lucas Satabin
 */
class LevenshteinMatcher(f: Double, t: Double) extends Matcher(f, t) {

  def distance(str1: String, str2: String): Int =
    (str1, str2) match {
      // degenerated cases
      case (s1, s2) if s1 == s2 => 0
      case ("", _)              => str2.length
      case (_, "")              => str1.length
      // standard case
      case (_, _)               =>
        var dist = (
           Array.ofDim[Int](str1.length + 1),
           Array.ofDim[Int](str1.length + 1)
        )

        for(idx <- 0 to str1.length)
          dist._2(idx) = idx

        for(jdx <- 1 to str2.length) {
          val (newDist, oldDist) = dist
          newDist(0) = jdx
          for (idx <- 1 to str1.length) {
            newDist(idx) = minimum (
              oldDist(idx) + 1,
              newDist(idx-1) + 1,
              oldDist(idx-1) + (if (str1(idx-1) == str2(jdx-1)) 0 else 1)
            )
          }
          dist = (oldDist, newDist)
        }

      dist._2(str1.length)
    }

  private def minimum(i1: Int, i2: Int, i3: Int) =
    math.min(math.min(i1, i2), i3)

}

