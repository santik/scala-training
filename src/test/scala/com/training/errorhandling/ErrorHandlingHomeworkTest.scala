package com.training.errorhandling

import cats.data.Validated
import com.training.errorhandling.ErrorHandlingHomework.ValidationError._
import com.training.errorhandling.ErrorHandlingHomework._
import com.training.errorhandling.ErrorHandlingHomeworkTest._
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.time.YearMonth

class ErrorHandlingHomeworkTest
  extends AnyFreeSpec
    with Matchers {

  "Number should" - {
    "accept valid strings" in {
      checkValid(Number.fromString("12345678"), paymentCard.number)
    }
    "reject invalid strings and report all encountered errors" in {
      checkInvalid(Number.fromString("ABC"), Set(NumberIsOutOfRange, NumberContainsInvalidCharacters))
      checkInvalid(Number.fromString("0123456789"), Set(NumberStartsWithZero))
    }
  }

  "ExpirationDate should" - {
    "accept valid strings" in {
      checkValid(ExpirationDate.fromString("02/2027")(nowJan2025), paymentCard.expirationDate)
    }
    "reject invalid strings and report all encountered errors" in {
      checkInvalid(ExpirationDate.fromString("AB/CD")(nowJan2025), Set(ExpirationDateHasWrongFormat))
      checkInvalid(ExpirationDate.fromString("11/2001")(nowJan2025), Set(ExpirationDateIsInThePast))
    }
  }

  "Name should" - {
    "accept valid strings" in {
      checkValid(Name.fromString("John Doe"), paymentCard.name)
    }
    "reject invalid strings and report all encountered errors" in {
      checkInvalid(Name.fromString(" J"), Set(NameStartsOrEndsWithSpace))
      checkInvalid(Name.fromString("@"), Set(NameIsOutOfRange, NameContainsInvalidCharacters))
    }
  }

  "SecurityCode should" - {
    "accept valid strings" in {
      checkValid(SecurityCode.fromString("012"), paymentCard.securityCode)
    }
    "reject invalid strings and report all encountered errors" in {
      checkInvalid(SecurityCode.fromString("ABC"), Set(SecurityCodeIsInvalid))
    }
  }

  "PaymentCardValidator should" - {
    "accept valid strings" in {
      checkValid(PaymentCardValidator.validate(
        number = "12345678",
        expirationDate = "02/2027",
        name = "John Doe",
        securityCode = "012",
      )(nowJan2025), paymentCard)
    }
    "reject invalid strings and report all encountered errors" in {
      checkInvalid(PaymentCardValidator.validate(
        number = "ABC",
        expirationDate = "AB/CD",
        name = "@",
        securityCode = "ABC",
      )(nowJan2025), Set(
        NumberIsOutOfRange,
        NumberContainsInvalidCharacters,
        ExpirationDateHasWrongFormat,
        NameIsOutOfRange,
        NameContainsInvalidCharacters,
        SecurityCodeIsInvalid,
      ))
    }
  }

  private def checkValid[A](result: AllErrorsOr[A], expected: A): Assertion =
    result match {
      case Validated.Valid(a)   => a shouldBe expected
      case Validated.Invalid(e) => fail(message = s"Expected $expected, but got $e")
    }

  private def checkInvalid[A](result: AllErrorsOr[A], expected: Set[ValidationError]): Assertion =
    result match {
      case Validated.Valid(a)   => fail(message = s"Expected $expected, but got $a")
      case Validated.Invalid(e) => e.iterator.toSet shouldBe expected
    }
}

object ErrorHandlingHomeworkTest {

  private val nowJan2025 = () => YearMonth.of(2025, 1)

  private val paymentCard = PaymentCard(
    number = Number("12345678"),
    expirationDate = ExpirationDate(YearMonth.of(2027, 2)),
    name = Name("John Doe"),
    securityCode = SecurityCode("012"),
  )
}
