package testing

import org.scalacheck.Gen
import org.scalacheck.Test.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  import Json._

  implicit val params: Parameters = Parameters.defaultVerbose.withMinSuccessfulTests(1000)

  def nullGen: Gen[Json] = Gen.const(JNull)
  def booleanGen: Gen[JBoolean] = Gen.oneOf(true, false).map(JBoolean)
  def numericGen: Gen[JNumber] = Gen.double.map(JNumber)
  def stringGen: Gen[JString] = Gen.alphaNumStr.map(JString)
  def arrayGen: Gen[JArray] = Gen.listOfN(3, Gen.lzy(jsonGen)).map(_.toVector).map(JArray)
  def objectGen: Gen[JObject] = {
    val gen = Gen.lzy(jsonGen).flatMap(x => Gen.identifier.map((_, x)))
    Gen.mapOfN(3, gen).map(JObject)
  }

  def jsonGen: Gen[Json] = Gen.oneOf(nullGen, booleanGen, numericGen, stringGen, arrayGen, objectGen)

  "parse" should "invert print" in {
    forAll(jsonGen) { x =>
      parse(print(x)) should be(Some(x))
    }
  }
}
