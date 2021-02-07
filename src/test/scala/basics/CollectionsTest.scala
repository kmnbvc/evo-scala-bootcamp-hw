package basics

import basics.Collections.SolutionBalancedStringSplit.balancedStringSplit
import basics.Collections.SolutionKidsWithCandies.kidsWithCandies
import basics.Collections.SolutionMatrixBlockSum.matrixBlockSum
import basics.Collections.SolutionMaxDepth.maxDepth
import basics.Collections.SolutionMaxWealth.maximumWealth
import basics.Collections.SolutionMaxWidthOfVerticalArea.maxWidthOfVerticalArea
import basics.Collections.SolutionRunningSum.runningSum
import basics.Collections.SolutionShuffle.shuffle
import org.scalatest.flatspec._
import org.scalatest.matchers.should

class CollectionsTest extends AnyFlatSpec with should.Matchers {

  ignore can "run example" in {
    println(runningSum(Array()).mkString("Array(", ", ", ")"))
    println(runningSum(Array(3, 1, 2, 10, 1)).mkString("Array(", ", ", ")"))

    println(shuffle(Array(2, 5, 1, 3, 4, 7), 3).mkString("Array(", ", ", ")"))

    println(maximumWealth(Array(Array(2, 8, 7), Array(7, 1, 3), Array(1, 9, 5))))

    println(kidsWithCandies(Array(2, 3, 5, 1, 3), 3).mkString("Array(", ", ", ")"))

    println(maxWidthOfVerticalArea(Array(Array(8, 7), Array(9, 9), Array(7, 4), Array(9, 7))))
    println(maxWidthOfVerticalArea(Array(Array(3, 1), Array(9, 0), Array(1, 0), Array(1, 4), Array(5, 3), Array(8, 8))))

    println(maxDepth("(1+(2*3)+((8)/4))+1"))
    println(maxDepth("(1)+((2))+(((3)))"))
    println(maxDepth("1+(2*3)/(2-1)"))
    println(maxDepth("1"))


    println(balancedStringSplit("RLRRLLRLRL"))
    println(balancedStringSplit("RLLLLRRRLR"))
    println(balancedStringSplit("LLLLRRRR"))
    println(balancedStringSplit("RLRRRLLRLL"))

    println(matrixBlockSum(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 1).map(_.toList).toList)
    println(matrixBlockSum(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 2).map(_.toList).toList)
  }

}
