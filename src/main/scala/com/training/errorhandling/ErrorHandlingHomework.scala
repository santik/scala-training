package com.training.errorhandling

import cats.data.ValidatedNec
import cats.syntax.all._
import com.training.errorhandling.ErrorHandlingHomework.ValidationError._

import java.time.YearMonth

/** Implement all methods marked with `???` below according to their Scaladoc description.
  * Add helper methods and more unit tests if necessary to properly verify the solution.
  */
object ErrorHandlingHomework {

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  /** Payment card number. */
  final case class Number(value: String) extends AnyVal
  object Number {


    /** Constructs [[Number]] from the string.
      * <p/>
      * The string must contain between 8 and 19 digits (inclusive) and must not start with 0.
      */
    def fromString(s: String): AllErrorsOr[Number] = {

      def validateNumberLength: AllErrorsOr[String] =
        if (s.length >= 8 && s.length <= 19) s.validNec
        else NumberIsOutOfRange.invalidNec

      def validateNumberStartsWithZero: AllErrorsOr[String] =
        if (!s.startsWith("0")) s.validNec
        else NumberStartsWithZero.invalidNec

      def validateNumberContainsDigits: AllErrorsOr[String] =
        if (s.matches("^\\d+$")) s.validNec
        else NumberContainsInvalidCharacters.invalidNec

      val f: String => Number = s => Number(s)

      validateNumberLength.productL(validateNumberStartsWithZero).productL(validateNumberContainsDigits).map(f)
    }
  }

  /** Payment card expiration date. */
  final case class ExpirationDate(value: YearMonth) extends AnyVal
  object ExpirationDate {



    /** Constructs [[ExpirationDate]] from the string.
      * <p/>
      * The string must be in the format `MM/YYYY`. `MM` part must be a pair of digits from "01" to "12".
      * `YYYY` part must be a pair of digits from "0001" to "9999". For example, `02/2024` means the card
      * is active until the last day of February 2024.
      * <p/>
      * [[ExpirationDate]] must not be in the past, i.e. before the current [[YearMonth]], as supplied
      * by the `now` function.
      *
      * @param now the function that returns the current [[YearMonth]]
      */
    def fromString(s: String)(now: () => YearMonth): AllErrorsOr[ExpirationDate] = {

      import java.time.format.DateTimeFormatter
      val format = DateTimeFormatter.ofPattern("MM/yyyy")

      def validateFormat: AllErrorsOr[String] = {
        val regex = "^(0[1-9]|1[0-2])\\/\\d{4}$"
        if (s.matches(regex)) s.validNec
        else ExpirationDateHasWrongFormat.invalidNec
      }
      def validateFuture: AllErrorsOr[String] = {
        if (!YearMonth.parse(s, format).isBefore(now())) s.validNec
        else ExpirationDateIsInThePast.invalidNec
      }

      val f: String => ExpirationDate = s => ExpirationDate(YearMonth.parse(s, format))
      validateFormat.andThen(_ => validateFuture).map(f)
    }
  }

  /** Payment card holder name. */
  final case class Name(value: String) extends AnyVal
  object Name {

    /** Constructs [[Name]] from the string.
      * <p/>
      * The string must be between 2 and 26 characters (inclusive). It must contain only spaces, upper and
      * lower case letters. It must not start or end with a space.
      */
    def fromString(s: String): AllErrorsOr[Name] = {
      def validateNameLength: AllErrorsOr[String] =
        if (s.length >= 2 && s.length <= 26) s.validNec
        else NameIsOutOfRange.invalidNec

      def validateNameSpaces: AllErrorsOr[String] =
        if (s.trim == s) s.validNec
        else NameStartsOrEndsWithSpace.invalidNec

      def validateFormat: AllErrorsOr[String] = {
        val regex = "^[a-zA-Z ]*$"
        if (s.matches(regex)) s.validNec
        else NameContainsInvalidCharacters.invalidNec
      }

      val f: String => Name = s => Name(s)
      validateNameLength.productL(validateNameSpaces).productL(validateFormat).map(f)
    }
  }

  /** Payment card security code. */
  final case class SecurityCode(value: String) extends AnyVal
  object SecurityCode {

    /** Constructs [[SecurityCode]] from the string.
      * <p/>
      * The string must contain between 3 and 4 digits (inclusive).
      */
    def fromString(s: String): AllErrorsOr[SecurityCode] = {
      def validateFormat: AllErrorsOr[String] = {
        val regex = "^\\d{3,4}$"
        if (s.matches(regex)) s.validNec
        else SecurityCodeIsInvalid.invalidNec
      }

      val f: String => SecurityCode = s => SecurityCode(s)
      validateFormat.map(f)
    }
  }

  /** Payment card. */
  final case class PaymentCard(
    number: Number,
    expirationDate: ExpirationDate,
    name: Name,
    securityCode: SecurityCode,
  )

  /** Payment card validation error. */
  sealed trait ValidationError
  object ValidationError {

    /** Payment card number contains less than 8 or more than 19 characters. */
    final case object NumberIsOutOfRange extends ValidationError

    /** Payment card number contains characters other than digits. */
    final case object NumberContainsInvalidCharacters extends ValidationError

    /** Payment card number starts with 0. */
    final case object NumberStartsWithZero extends ValidationError

    /** Expiration date is not in the format "MM/YYYY". */
    final case object ExpirationDateHasWrongFormat extends ValidationError

    /** Expiration date is in the past, i.e. earlier than the current month of the current year. */
    final case object ExpirationDateIsInThePast extends ValidationError

    /** Payment card name contains less than 2 or more than 26 characters. */
    final case object NameIsOutOfRange extends ValidationError

    /** Payment card name starts or ends with a space. */
    final case object NameStartsOrEndsWithSpace extends ValidationError

    /** Payment card name contains characters other than spaces, upper and lower case letters. */
    final case object NameContainsInvalidCharacters extends ValidationError

    /** Payment card security code is invalid. */
    final case object SecurityCodeIsInvalid extends ValidationError
  }

  object PaymentCardValidator {

    /** Attempts to construct [[PaymentCard]] from the supplied raw strings. Aggregates all encountered
      * validation errors. Returns as many errors as possible for any given set of input parameters.
      * <p/>
      * For example, if the supplied number is "0ABC", while the security code is "DEF", the returned errors
      * must include [[NumberIsOutOfRange]], [[NumberContainsInvalidCharacters]], [[NumberStartsWithZero]]
      * and [[SecurityCodeIsInvalid]].
      *
      * @param now the function that returns the current [[YearMonth]]
      *
      * @return [[PaymentCard]] or all encountered validation errors.
      */
    def validate(
      number: String,
      expirationDate: String,
      name: String,
      securityCode: String,
    )(now: () => YearMonth): AllErrorsOr[PaymentCard] = {
      (
        Number.fromString(number),
        ExpirationDate.fromString(expirationDate)(now),
        Name.fromString(name),
        SecurityCode.fromString(securityCode)
        ).mapN(PaymentCard)
    }
  }
}
