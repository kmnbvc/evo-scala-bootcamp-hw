package error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxOption, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.Try

object ErrorHandling {

  import PaymentCardValidator._
  import ValidationError._

  final case class CardNumber private(value: String) extends AnyVal
  object CardNumber {
    def apply(value: String): AllErrorsOr[CardNumber] = {
      def validateLength: AllErrorsOr[String] = if (value.length != 16) InvalidCardNumberLength.invalidNec else value.validNec
      def validateFormat: AllErrorsOr[String] = if (!value.matches("[1-9]+")) InvalidCardNumberFormat.invalidNec else value.validNec

      validateLength.ap(validateFormat.map(passthroughValid[String])).map(new CardNumber(_))
    }
  }

  final case class CardSecurityCode private(value: String) extends AnyVal
  object CardSecurityCode {
    def apply(value: String): AllErrorsOr[CardSecurityCode] = {
      def validateLength: AllErrorsOr[String] = if (value.length != 3) InvalidSecurityCodeLength.invalidNec else value.validNec
      def validateFormat: AllErrorsOr[String] = if (!value.matches("[1-9]+")) InvalidSecurityCodeFormat.invalidNec else value.validNec

      validateLength.ap(validateFormat.map(passthroughValid[String])).map(new CardSecurityCode(_))
    }
  }

  final case class CardOwnerName private(value: String) extends AnyVal
  object CardOwnerName {
    def apply(value: String): AllErrorsOr[CardOwnerName] = {
      def validateCase: AllErrorsOr[String] = if (value.exists(_.isLower)) InvalidOwnerNameCase.invalidNec else value.validNec
      def validateMaxLength: AllErrorsOr[String] = if (value.length > 99) OwnerNameMaxLengthExceeded.invalidNec else value.validNec
      def validateChars: AllErrorsOr[String] = if (!value.matches("[a-zA-Z]+")) InvalidOwnerNameChars.invalidNec else value.validNec

      validateCase.ap(validateMaxLength.map(passthroughValid[String]))
        .ap(validateChars.map(passthroughValid[String]))
        .map(new CardOwnerName(_))
    }
  }

  final case class CardExpirationDate private(value: YearMonth) extends AnyVal
  object CardExpirationDate {
    private val formatter = DateTimeFormatter.ofPattern("MM/yyyy")

    def apply(value: String): AllErrorsOr[CardExpirationDate] = {
      def validateFormat: AllErrorsOr[YearMonth] = Try(YearMonth.parse(value, formatter)).toOption.toValidNec(InvalidExpirationDateFormat)
      def validateDateNotExpired(value: YearMonth): AllErrorsOr[YearMonth] = if (value.isBefore(YearMonth.now())) DateAlreadyExpired.invalidNec else value.validNec

      validateFormat.andThen(validateDateNotExpired).map(new CardExpirationDate(_))
    }
  }

  final case class PaymentCard(name: CardOwnerName,
                               number: CardNumber,
                               securityCode: CardSecurityCode,
                               expire: CardExpirationDate)

  sealed trait ValidationError
  object ValidationError {
    case object InvalidCardNumberFormat extends ValidationError
    case object InvalidCardNumberLength extends ValidationError

    case object InvalidOwnerNameChars extends ValidationError
    case object InvalidOwnerNameCase extends ValidationError
    case object OwnerNameMaxLengthExceeded extends ValidationError

    case object InvalidSecurityCodeFormat extends ValidationError

    case object InvalidSecurityCodeLength extends ValidationError

    case object InvalidExpirationDateFormat extends ValidationError
    case object DateAlreadyExpired extends ValidationError

    case object RequiredValue extends ValidationError
  }

  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(name: String,
                 number: String,
                 expirationDate: String,
                 securityCode: String): AllErrorsOr[PaymentCard] = {
      (CardOwnerName(name),
        CardNumber(number),
        CardSecurityCode(securityCode),
        CardExpirationDate(expirationDate)).mapN(PaymentCard)
    }
  }

  private def passthroughValid[A]: A => A => A = _ => identity
}
