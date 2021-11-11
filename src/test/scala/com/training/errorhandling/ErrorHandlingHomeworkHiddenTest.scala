package com.training.errorhandling

import cats.data.Validated
import org.scalacheck.{Gen, Shrink}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.training.errorhandling.ErrorHandlingHomework._
import com.training.errorhandling.ErrorHandlingHomework.PaymentCardValidator
import com.training.errorhandling.ErrorHandlingHomework.ValidationError._
import com.training.errorhandling.ErrorHandlingHomeworkHiddenTest._
import org.scalatest.Assertion

import java.time.YearMonth

class ErrorHandlingHomeworkHiddenTest
  extends AnyFreeSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  // Do not shrink generated strings
  implicit val noShrinkString: Shrink[String] = Shrink.shrinkAny

  "PaymentCardValidator should" - {
    "accept valid strings" in {
      forAll(Generators.paymentCardGen) { paymentCard =>
        val now = () => paymentCard.expirationDate.value
        checkValid(PaymentCardValidator.validate(
          number = paymentCard.number.value,
          expirationDate = paymentCard.expirationDate.format,
          name = paymentCard.name.value,
          securityCode = paymentCard.securityCode.value,
        )(now), paymentCard)
      }
    }
    "reject invalid strings and report all encountered errors" in {
      forAll(
        Generators.invalidNumberGen,
        Generators.expirationDateGen,
        Generators.invalidExpirationDateGen,
        Generators.invalidNameGen,
        Generators.invalidSecurityCodeGen,
      ) { (invalidNumber, expirationDate, invalidExpirationDate, invalidName, invalidSecurityCode) =>
        val now = () => expirationDate.value.plusMonths(1)

        checkInvalid(PaymentCardValidator.validate(
          number = invalidNumber,
          expirationDate = invalidExpirationDate,
          name = invalidName,
          securityCode = invalidSecurityCode,
        )(now), Set(
          NumberIsOutOfRange,
          NumberContainsInvalidCharacters,
          NumberStartsWithZero,
          ExpirationDateHasWrongFormat,
          NameIsOutOfRange,
          NameStartsOrEndsWithSpace,
          NameContainsInvalidCharacters,
          SecurityCodeIsInvalid,
        ))

        checkInvalid(PaymentCardValidator.validate(
          number = invalidNumber,
          expirationDate = expirationDate.format,
          name = invalidName,
          securityCode = invalidSecurityCode,
        )(now), Set(
          NumberIsOutOfRange,
          NumberContainsInvalidCharacters,
          NumberStartsWithZero,
          ExpirationDateIsInThePast,
          NameIsOutOfRange,
          NameStartsOrEndsWithSpace,
          NameContainsInvalidCharacters,
          SecurityCodeIsInvalid,
        ))
      }
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

object ErrorHandlingHomeworkHiddenTest {

  object Generators {

    private def stringGen(
      min: Int,
      max: Int,
      charGen: Gen[Char],
    ): Gen[String] =
      for {
        size <- Gen.chooseNum(minT = min, maxT = max)
        chars <- Gen.listOfN(size, charGen)
      } yield chars.mkString

    val numberGen: Gen[Number] =
      for {
        head <- Gen.chooseNum(minT = 1, maxT = 9)
        tail <- stringGen(min = 7, max = 18, charGen = Gen.numChar)
      } yield Number(head.toString + tail)

    val expirationDateGen: Gen[ExpirationDate] =
      for {
        month <- Gen.chooseNum(minT = 1, maxT = 12)
        year <- Gen.chooseNum(minT = 1, maxT = 9999)
      } yield ExpirationDate(YearMonth.of(year, month))

    val nameGen: Gen[Name] =
      for {
        first <- Gen.alphaChar
        middle <- stringGen(min = 0, max = 24, charGen = Gen.oneOf(Gen.alphaChar, Gen.const(' ')))
        last <- Gen.alphaChar
      } yield Name(first + middle + last)

    val securityCodeGen: Gen[SecurityCode] =
      stringGen(
        min = 3,
        max = 4,
        charGen = Gen.numChar,
      ).map(SecurityCode(_))

    val paymentCardGen: Gen[PaymentCard] = for {
      number <- numberGen
      expirationDate <- expirationDateGen
      name <- nameGen
      securityCode <- securityCodeGen
    } yield PaymentCard(number, expirationDate, name, securityCode)

    val invalidNumberGen: Gen[String] =
      for {
        head <- Gen.const('0')
        tail <- stringGen(min = 1, max = 6, charGen = Gen.alphaChar)
      } yield head + tail

    val invalidExpirationDateGen: Gen[String] =
      for {
        month <- Gen.oneOf(
          Gen.const("00"),
          Gen.chooseNum(minT = 13, maxT = 99).map(_.toString),
          stringGen(min = 2, max = 2, charGen = Gen.alphaChar),
        )
        year <- Gen.oneOf(
          Gen.const("0000"),
          stringGen(min = 5, max = 8, charGen = Gen.numChar),
          stringGen(min = 4, max = 4, charGen = Gen.alphaChar),
        )
      } yield month + "/" + year

    val invalidNameGen: Gen[String] =
      for {
        first <- Gen.const(' ')
        middle <- stringGen(min = 25, max = 48, charGen = Gen.alphaNumChar)
        last <- Gen.oneOf('@', '!', '&', '*')
      } yield first + middle + last

    val invalidSecurityCodeGen: Gen[String] = {
      Gen.oneOf(
        stringGen(min = 3, max = 4, charGen = Gen.alphaChar),
        stringGen(min = 5, max = 25, charGen = Gen.numChar),
      )
    }
  }

  implicit class ExpirationDateFormatter(expirationDate: ExpirationDate) {

    private def prependZeros(s: String, desiredLength: Int): String = {
      val delta = desiredLength - s.length
      if (delta > 0) "0" * delta + s else s
    }

    def format: String = {
      val month = expirationDate.value.getMonthValue.toString
      val year = expirationDate.value.getYear.toString
      prependZeros(month, desiredLength = 2) + "/" + prependZeros(year, desiredLength = 4)
    }
  }
}
