package basics

import Basics._
import org.scalatest._
import flatspec._
import matchers._

class BasicsSpec extends AnyFlatSpec with should.Matchers {

  "lcm" should "calc" in {
    lcm(0, 0) should be(0)
    lcm(0, 2) should be(0)
    lcm(3, 2) should be(6)
    lcm(4, 5) should be(20)
    lcm(12, 18) should be(36)
    lcm(72, 120) should be(360)
    lcm(List(60,18,28)) should be(1260)
    lcm(List(6, 11, 18)) should be(198)
    lcm(List(8, 3, 5)) should be(120)
    lcm(List(10)) should be(10)
  }

  "gcd" should "calc" in {
    gcd(42, 56) should be(14)
    gcd(461952, 116298) should be(18)
    gcd(7966496, 314080416) should be(32)
    gcd(24826148, 45296490) should be(526)
    gcd(12, 0) should be(12)
    gcd(0, 0) should be(0)
    gcd(0, 9) should be(9)
  }

}
