package basics

import scala.annotation.tailrec

object Collections {

  // hometask:
  // https://leetcode.com/problems/running-sum-of-1d-array/
  object SolutionRunningSum {
    def runningSum(nums: Array[Int]): Array[Int] = {
      nums.scanLeft(0)(_ + _).tail
    }
  }

  // https://leetcode.com/problems/shuffle-the-array
  object SolutionShuffle {
    def shuffle(nums: Array[Int], n: Int): Array[Int] = {
      nums.zip(nums.drop(n)).foldLeft(List[Int]()) {
        case (acc, (x, y)) => x :: y :: acc
      }.reverse.toArray
    }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  object SolutionMaxWealth {
    def maximumWealth(accounts: Array[Array[Int]]): Int = {
      accounts.map(_.sum).max
    }
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  object SolutionKidsWithCandies {
    def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
      candies.map(_ + extraCandies >= candies.max)
    }
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  object SolutionMaxWidthOfVerticalArea {
    def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
      points.map(_.head).sorted.sliding(2).map(a => a.last - a.head).max
    }
  }

  // optional hometask:
  //
  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  object SolutionMaxDepth {
    def maxDepth(s: String): Int = {
      s.map {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }.scanLeft(0)(_ + _).max
    }
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  object SolutionBalancedStringSplit {
    def balancedStringSplit(s: String): Int = {
      @tailrec def helper(s: String, count: Int, ls: Int, rs: Int): Int =
        s.headOption match {
          case _ if ls != 0 && ls == rs => helper(s, count + 1, 0, 0)
          case Some('R') => helper(s.tail, count, ls, rs + 1)
          case Some('L') => helper(s.tail, count, ls + 1, rs)
          case None => count
        }

      helper(s, 0, 0, 0)
    }
  }

  // https://leetcode.com/problems/matrix-block-sum/
  object SolutionMatrixBlockSum {
    def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
      val (m, n) = (mat.length, mat(0).length)
      val result = for {
        i <- Array.range(0, m)
        j <- Array.range(0, n)
        rs = (i - K to i + K).filter(r => r >= 0 && r < m)
        cs = (j - K to j + K).filter(c => c >= 0 && c < n)
        y = rs.flatMap(r => cs.map(c => mat(r)(c)))
      } yield y.sum

      result.sliding(m, m).toArray
    }
  }

}
