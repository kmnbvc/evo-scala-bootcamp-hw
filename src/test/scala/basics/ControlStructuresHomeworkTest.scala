package basics

import basics.ControlStructuresHomework._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ControlStructuresHomeworkTest extends AnyFlatSpec with should.Matchers {

  "sum" should "calc correctly" in {
    process("sum 5 5 6 8.5") should be("the sum of 5 5 6 8.5 is 24.5")
  }

  "average" should "calc correctly" in {
    process("average 4 3 8.5 4") should be("the average of 4 3 8.5 4 is 4.875")
  }

  "divide" should "calc correctly" in {
    process("divide 4 5") should be("4 divided by 5 is 0.8")
  }

  "min" should "calc correctly" in {
    process("min 4 -3 -17") should be("the minimum of 4 -3 -17 is -17")
  }

  "max" should "calc correctly" in {
    process("max 4 -3 -17") should be("the maximum of 4 -3 -17 is 4")
  }

  "divide by 0" should "return error" in {
    process("divide 1 0") should be("Error: / by 0")
  }

  "divide with more or less than 2 numbers" should "return error" in {
    process("divide 1 2 3") should be("Error: this command takes 2 numbers")
    process("divide 1") should be("Error: this command takes 2 numbers")
  }

  "unknown command" should "return error" in {
    process("ln 0") should be("Error: unknown command")
  }

  "if no numbers" should "return error" in {
    process("sum") should be("Error: nothing to calculate")
  }

  "empty string" should "return error" in {
    process("") should be("Error: nothing to calculate")
  }

}
