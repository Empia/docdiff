package gnieh.docdiff

import gnieh.string.{
  SuffixArray,
  LongestCommonPrefix
}
import scala.annotation.tailrec

import scala.collection.mutable.ListBuffer

abstract class NOCS(val minLength: Int) extends SuffixArray with LongestCommonPrefix {

  def isValid(start: Int, lenth: Int): Boolean

  def findNOCSs(ls1: String, ls2: String): Vector[CS] = {
    val n1 = ls1.length
    val n2 = ls2.length
    val input = ls1 + 1.toChar + ls2
    val sa = suffixArray(input)
    val lcp = lcps(input, sa)
    val buckets = saLcpBucketSort(sa, lcp, n1)
    greedyCover(buckets, n1, n2)
  }

  private def saLcpBucketSort(sa: Vector[Int], lcp: Vector[Int], n1: Int): Vector[Vector[CS]] = {
    val buckets = Array.ofDim[Vector[CS]](sa.length)
    @tailrec
    def loop(i: Int): Unit =
      if(i < lcp.length) {
        val len = lcp(i)
        if(len >= minLength) {
          // only accept long enough strings
          val start1 = sa(i - 1)
          val start2 = sa(i)
          // skip duplicates
          var j = i + 1
          while(j < lcp.length && lcp(j) == len) {
            j += 1
          }

          if(j > i + 1) {
            loop(j)
          } else if((start1 < n1) == (start2 < n1)) {
            // only accept substrings of both strings
            loop(i + 1)
          } else {
            // correct start1 and start2
            val (start11, start21) =
              if(start2 < start1)
                (start2, start1)
              else
                (start1, start2)
            val start22 = start21 - (n1 + 1)
            if(buckets(len) == null) {
              buckets(len) = Vector.empty
            }
            buckets(len) = buckets(len) :+ (start1, start2, len)
            loop(i + 1)
          }
        } else {
          loop(i + 1)
        }
      }
    loop(1)
    buckets.toVector
  }

  private def greedyCover(buckets: Vector[Vector[CS]], n1: Int, n2: Int) = {
    // initialize cover arrays
    val covered1 = Array.fill[Boolean](n1)(false)
    val covered2 = Array.fill[Boolean](n2)(false)
    val result = ListBuffer.empty[CS]
    // from longest to shortest substring
    for(bucket <- buckets.reverse) {
      @tailrec
      def loop(i: Int): Unit =
        if(i < bucket.length) {
          val (start1, start2, len) = bucket(i)
          if(covered1(start1) || covered2(start2) || !isValid(start1, len)) {
            loop(i + 1)
          } else {
            @tailrec
            def loopCs(j: Int): CS =
              if(j < len) {
                bucket(i)
              } else {
                val k1 = start1 + j
                val k2 = start2 + j
                if(covered1(k1) || covered2(k2)) {
                  // This one runs into another accepted substring
                  // It cannot be a longer prefix to that substring because we sorted by size and
                  // are already done with the longer substring. Therefore this substring is only
                  // a prefix in one of the strings but not in the other.
                  (start1, start2, j)
                } else {
                  covered1(k1) = true
                  covered2(k2) = true
                  loopCs(j + 1)
                }
              }
            val cs1 = loopCs(0)
            result.append(cs1)
          }
        }
      loop(0)
    }
    result.toVector
  }

}
