package error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}

import java.time.ZonedDateTime
import scala.util.Try

object ErrorHandling {

  import PaymentCardValidator._
  import ValidationError._

  final case class CardNumber private(value: String) extends AnyVal
  object CardNumber {
    def apply(value: String): AllErrorsOr[CardNumber] = {
      if (value.length != 16) InvalidCardNumberLength.invalidNec
      else if (!value.matches("[1-9]")) InvalidCardNumberFormat.invalidNec
      else new CardNumber(value).validNec
    }
  }

  final case class CardSecurityCode private(value: String) extends AnyVal
  object CardSecurityCode {
    def apply(value: String): AllErrorsOr[CardSecurityCode] = {
      if (value.length != 3) InvalidSecurityCodeLength.invalidNec
      else if (!value.matches("[1-9]")) InvalidSecurityCodeChars.invalidNec
      else new CardSecurityCode(value).validNec
    }
  }

  final case class CardName private(value: String) extends AnyVal
  object CardName {
    def apply(value: String): AllErrorsOr[CardName] = {
      if (value.exists(_.isLower)) InvalidNameCase.invalidNec
      else if (value.length > 99) NameMaxLengthExceeded.invalidNec
      else if (!value.matches("[A-Z]")) InvalidNameChars.invalidNec
      else new CardName(value).validNec
    }
  }

  final case class CardExpirationDate private(value: ZonedDateTime) extends AnyVal
  object CardExpirationDate {
    def apply(value: String): AllErrorsOr[CardExpirationDate] = {
      Try(ZonedDateTime.parse(value)).toEither.left.map(_ => InvalidDateFormat).flatMap(d =>
        if (d.isBefore(ZonedDateTime.now())) Left(DateAlreadyExpired)
        else Right(CardExpirationDate(d))
      ).fold(_.invalidNec, _.validNec)
    }
  }

  final case class PaymentCard(name: CardName,
                               number: CardNumber,
                               securityCode: CardSecurityCode,
                               expire: CardExpirationDate)

  sealed trait ValidationError
  object ValidationError {
    sealed case object InvalidCardNumberFormat extends ValidationError
    sealed case object InvalidCardNumberLength extends ValidationError

    sealed case object InvalidNameChars extends ValidationError
    sealed case object InvalidNameCase extends ValidationError
    sealed case object NameMaxLengthExceeded extends ValidationError

    sealed case object InvalidSecurityCodeChars extends ValidationError
    sealed case object InvalidSecurityCodeLength extends ValidationError

    sealed case object InvalidDateFormat extends ValidationError
    sealed case object DateAlreadyExpired extends ValidationError

    sealed case object RequiredValue extends ValidationError
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
