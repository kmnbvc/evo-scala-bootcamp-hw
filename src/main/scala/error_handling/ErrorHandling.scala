package error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxOption, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}

import java.time.ZonedDateTime
import scala.util.Try

object ErrorHandling {

  import PaymentCardValidator._
  import ValidationError._

  final case class CardNumber private(value: String) extends AnyVal
  object CardNumber {
    def apply(value: String): AllErrorsOr[CardNumber] = {
      def validateLength: AllErrorsOr[String] = if (value.length != 16) InvalidCardNumberLength.invalidNec else value.validNec
      def validSymbols(value: String): AllErrorsOr[String] = if (!value.matches("[1-9]")) InvalidCardNumberFormat.invalidNec else value.validNec

      validateLength.andThen(validSymbols).map(new CardNumber(_))
    }
  }

  final case class CardSecurityCode private(value: String) extends AnyVal
  object CardSecurityCode {
    def apply(value: String): AllErrorsOr[CardSecurityCode] = {
      def validLength: AllErrorsOr[String] = if (value.length != 3) InvalidSecurityCodeLength.invalidNec else value.validNec
      def validSymbols(value: String): AllErrorsOr[String] = if (!value.matches("[1-9]")) InvalidSecurityCodeChars.invalidNec else value.validNec

      validLength.andThen(validSymbols).map(new CardSecurityCode(_))
    }
  }

  final case class CardName private(value: String) extends AnyVal
  object CardName {
    def apply(value: String): AllErrorsOr[CardName] = {
      if (value.exists(_.isLower)) InvalidNameCase.invalidNec
      else if (value.length > 99) NameMaxLengthExceeded.invalidNec
      else if (!value.matches("[A-Z]")) InvalidNameChars.invalidNec
      else new CardName(value).validNec

      def validCase: AllErrorsOr[String] = if (value.exists(_.isLower)) InvalidNameCase.invalidNec else value.validNec
      def validMaxLength(value: String): AllErrorsOr[String] = if (value.length > 99) NameMaxLengthExceeded.invalidNec else value.validNec
      def validSymbols(value: String): AllErrorsOr[String] = if (!value.matches("[A-Z]")) InvalidNameChars.invalidNec else value.validNec

      validCase.andThen(validMaxLength).andThen(validSymbols).map(new CardName(_))
    }
  }

  final case class CardExpirationDate private(value: ZonedDateTime) extends AnyVal
  object CardExpirationDate {
    def apply(value: String): AllErrorsOr[CardExpirationDate] = {
      def validDateFormat: AllErrorsOr[ZonedDateTime] = Try(ZonedDateTime.parse(value)).toOption.toValidNec(InvalidDateFormat)
      def dateInFuture(date: ZonedDateTime): AllErrorsOr[ZonedDateTime] = if (date.isBefore(ZonedDateTime.now())) DateAlreadyExpired.invalidNec else date.validNec

      validDateFormat.andThen(dateInFuture).map(new CardExpirationDate(_))
    }
  }

  final case class PaymentCard(name: CardName,
                               number: CardNumber,
                               securityCode: CardSecurityCode,
                               expire: CardExpirationDate)

  sealed trait ValidationError
  object ValidationError {
    case object InvalidCardNumberFormat extends ValidationError
    case object InvalidCardNumberLength extends ValidationError

    case object InvalidNameChars extends ValidationError
    case object InvalidNameCase extends ValidationError
    case object NameMaxLengthExceeded extends ValidationError

    case object InvalidSecurityCodeChars extends ValidationError
    case object InvalidSecurityCodeLength extends ValidationError

    case object InvalidDateFormat extends ValidationError
    case object DateAlreadyExpired extends ValidationError

    case object RequiredValue extends ValidationError
  }

  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(name: String,
                 number: String,
                 expirationDate: String,
                 securityCode: String): AllErrorsOr[PaymentCard] = {
      (CardName(name),
        CardNumber(number),
        CardSecurityCode(securityCode),
        CardExpirationDate(expirationDate)).mapN(PaymentCard)
    }
  }

}
