package basics

import scala.io.Source
import scala.util.Try

object ControlStructuresHomework {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)
  final case class Result(cmd: Command, value: Double)

  import Command._

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    def parseNumbers(in: List[String]): Either[ErrorMessage, List[Double]] = Try(in.map(_.toDouble)).toOption.toRight(ErrorMessage("wrong numbers format"))

    x.split("\\s+").toList match {
      case Nil | _ :: Nil => Left(ErrorMessage("nothing to calculate"))
      case cmd :: nums =>
        parseNumbers(nums).flatMap(numbers => cmd match {
          case "divide" => Either.cond(numbers.length == 2, Divide(numbers.head, numbers.last), ErrorMessage("divide command takes 2 numbers"))
          case "sum" => Right(Sum(numbers))
          case "average" => Right(Average(numbers))
          case "min" => Right(Min(numbers))
          case "max" => Right(Max(numbers))
          case _ => Left(ErrorMessage("unknown command"))
        })
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case Divide(dividend, divisor) => Either.cond(divisor != 0, Result(x, dividend / divisor), ErrorMessage("/ by 0"))
    case Sum(numbers) => Right(Result(x, numbers.sum))
    case Average(numbers) => Right(Result(x, numbers.sum / numbers.length))
    case Min(numbers) => Right(Result(x, numbers.min))
    case Max(numbers) => Right(Result(x, numbers.max))
  }

  def renderResult(x: Result): String = x.cmd match {
    case Divide(dividend, divisor) => s"${mkString(dividend)} divided by ${mkString(divisor)} is ${mkString(x.value)}"
    case Sum(numbers) => render("sum", numbers, x.value)
    case Average(numbers) => render("average", numbers, x.value)
    case Min(numbers) => render("minimum", numbers, x.value)
    case Max(numbers) => render("maximum", numbers, x.value)
  }

  private def render(cmdName: String, numbers: List[Double], value: Double): String = {
    s"the $cmdName of ${mkString(numbers: _*)} is ${mkString(value)}"
  }

  private def mkString(values: Double*): String = values.map { value =>
    new java.math.BigDecimal(value.toString).stripTrailingZeros().toPlainString
  }.mkString(" ")

  def process(x: String): String = {
    val result = for {
      cmd <- parseCommand(x)
      r <- calculate(cmd)
    } yield renderResult(r)

    result.fold("Error: " + _.value, identity)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
