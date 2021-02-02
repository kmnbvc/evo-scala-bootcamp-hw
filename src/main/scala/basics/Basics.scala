package basics

import scala.annotation.tailrec

object Basics {

  def lcm(numbers: List[Int]): Int = numbers match {
    case Nil => 0
    case n :: Nil => n
    case n :: ns => lcm(lcm(ns), n)
  }

  def lcm(a: Int, b: Int): Int = if (a == 0 || b == 0) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

}
