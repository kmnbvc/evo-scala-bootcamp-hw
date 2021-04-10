package testing

import org.scalacheck.Gen
import org.scalacheck.Test.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  import Json._

  implicit val params: Parameters = Parameters.default.withMinSuccessfulTests(1000)

  def jsonGen: Gen[Json] = ???

  "parse" should "invert print" in {
    forAll(jsonGen) { _ =>

    }
  }
}
