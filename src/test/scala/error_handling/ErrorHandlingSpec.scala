package error_handling

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import error_handling.ErrorHandling.ValidationError._
import error_handling.ErrorHandling._
import org.scalatest.Inspectors.forAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should

import java.time.YearMonth

class ErrorHandlingSpec extends AnyFreeSpec with should.Matchers {

  "Card number" - {
    "should check length is 16" in {
      val values = List("111", "9875" * 99)
      forAll(values) { input =>
        CardNumber(input) should be(Invalid(NonEmptyChain(InvalidCardNumberLength)))
      }
    }
    "should check value is digits only" in {
      val values = List("123e" * 4, "#$%@" * 4, "1111-2222-3333-4")
      forAll(values) { input =>
        CardNumber(input) should be(Invalid(NonEmptyChain(InvalidCardNumberFormat)))
      }
    }
    "should collect all errors" in {
      val invalidNumber = "!@#$345" * 999
      CardNumber(invalidNumber) should be(Invalid(NonEmptyChain(InvalidCardNumberLength, InvalidCardNumberFormat)))
    }
    "should be no errors if value is valid" in {
      val value = "1" * 16
      CardNumber(value) should matchPattern { case Valid(CardNumber(x)) if x == value => }
    }
  }

  "Card security code" - {
    "should check length is 3" in {
      val values = List("1", "9875")
      forAll(values) { input =>
        CardSecurityCode(input) should be(Invalid(NonEmptyChain(InvalidSecurityCodeLength)))
      }
    }
    "should check value is digits only" in {
      val values = List("1a3", "#$%", "1,1")
      forAll(values) { input =>
        CardSecurityCode(input) should be(Invalid(NonEmptyChain(InvalidSecurityCodeFormat)))
      }
    }
    "should collect all errors" in {
      val invalidCode = "!@#$345"
      CardSecurityCode(invalidCode) should be(Invalid(NonEmptyChain(InvalidSecurityCodeLength, InvalidSecurityCodeFormat)))
    }
    "should be no errors if value is valid" in {
      val value = "123"
      CardSecurityCode(value) should matchPattern { case Valid(CardSecurityCode(x)) if x == value => }
    }
  }

  "Card owner name" - {
    "should validate letters is in upper case" in {
      val invalid = List("abc", "ABc", "AbC")
      forAll(invalid) { input =>
        CardOwnerName(input) should be(Invalid(NonEmptyChain(InvalidOwnerNameCase)))
      }
    }
    "should validate max length" in {
      val invalid = "A" * 100
      CardOwnerName(invalid) should be(Invalid(NonEmptyChain(OwnerNameMaxLengthExceeded)))
    }
    "should validate value is letters only" in {
      val invalid = List("EHFK435", "GJ%@")
      forAll(invalid) { input =>
        CardOwnerName(input) should be(Invalid(NonEmptyChain(InvalidOwnerNameChars)))
      }
    }
    "should collect all errors" in {
      val invalidName = "!@#$345abc" * 999
      CardOwnerName(invalidName) should be(Invalid(NonEmptyChain(InvalidOwnerNameCase,
        OwnerNameMaxLengthExceeded, InvalidOwnerNameChars)))
    }
  }

  "Card expiration date" - {
    "validate format" in {
      val invalid = List("11 12", "1234", "sdfsd", "11111")
      forAll(invalid) { input =>
        CardExpirationDate(input) should be(Invalid(NonEmptyChain(InvalidExpirationDateFormat)))
      }
    }
    "validate not expired" in {
      val invalid = "01/1999"
      CardExpirationDate(invalid) should be(Invalid(NonEmptyChain(DateAlreadyExpired)))
    }
    "should work for valid case" in {
      val input = "03/2021"
      val expected = YearMonth.of(2021, 3)
      CardExpirationDate(input) should matchPattern { case Valid(CardExpirationDate(d)) if d == expected => }
    }
  }

  "PaymentCardValidator.validate" - {
    "validate fields and collect all errors" in {
      PaymentCardValidator.validate("name", "card number 123", "01/02/2023", "sec code") should be(
        Invalid(NonEmptyChain(
          InvalidOwnerNameCase,
          InvalidCardNumberLength, InvalidCardNumberFormat,
          InvalidSecurityCodeLength, InvalidSecurityCodeFormat,
          InvalidExpirationDateFormat
        ))
      )
    }
  }

}
