package com.training.csvparser

import com.training.csvparser.CsvParser.Car.{EngineType, LicensePlateNumber, Manufacturer, Model, ProductionYear}
import com.training.csvparser.CsvParser.Csv.{Line, LineNumber}
import com.training.csvparser.CsvParser.ParsingFailure.Reason
import com.training.csvparser.CsvParser.SortCriteria.Field.{ByEngineType, ByLicensePlateNumber, ByManufacturer, ByModel, ByProductionYear}
import com.training.csvparser.CsvParser.SortCriteria.Order.{Ascending, Descending}
import com.training.csvparser.CsvParser.SortCriteria.{Field, Order}

import scala.util.{Success, Try}

/** Implement all methods marked with `???` below according to their Scaladoc description.
  * Add more unit tests if necessary to properly verify the solution.
  */
object CsvParser {

  /** CSV data consisting of individual lines.
    *
    * @param lines CSV data lines, do not contain header or footer lines
    */
  final case class Csv(lines: List[Line])

  object Csv {

    /** Zero-based CSV data line number. */
    final case class LineNumber(value: Int) extends AnyVal

    /** CSV data line. */
    final case class Line(value: String) extends AnyVal
  }

  /** Sort criteria.
    *
    * @param field field to sort according to
    * @param order sort order to use, string values are compared lexicographically
    */
  final case class SortCriteria(
                                 field: Field,
                                 order: Order,
                               )

  object SortCriteria {

    sealed trait Field

    object Field {
      final case object ByManufacturer extends Field

      final case object ByModel extends Field

      final case object ByProductionYear extends Field

      final case object ByLicensePlateNumber extends Field

      final case object ByEngineType extends Field
    }

    sealed trait Order

    object Order {
      final case object Ascending extends Order

      final case object Descending extends Order
    }
  }

  final case class Car(
                        manufacturer: Manufacturer,
                        model: Model,
                        productionYear: ProductionYear,
                        licensePlateNumber: LicensePlateNumber,
                        engineType: EngineType,
                      )

  object Car {

    /** Car manufacturer. */
    final case class Manufacturer(value: String) extends AnyVal

    object Manufacturer {

      /** Constructs [[Manufacturer]] from the string.
        * <p/>
        * The string must be between 2 and 100 characters, must not be blank.
        */
      def fromString(s: String): Option[Manufacturer] = {
        if (s.strip.nonEmpty && (2 to 100 contains s.length))
          Some(Manufacturer(s))
        else
          None
      }
    }

    /** Car model. */
    final case class Model(value: String) extends AnyVal

    object Model {

      /** Constructs [[Model]] from the string.
        * <p/>
        * The string must be between 1 and 200 characters, must not be blank.
        */
      def fromString(s: String): Option[Model] = {
        if (s.strip.nonEmpty && s.length <= 200 && s.nonEmpty)
          Some(Model(s))
        else
          None
      }
    }

    /** Car production year. */
    final case class ProductionYear(value: Int) extends AnyVal

    object ProductionYear {

      /** Constructs [[ProductionYear]] from the string.
        * <p/>
        * The string must contain a number between 1886 and 2100.
        */
      def fromString(s: String): Option[ProductionYear] = {
        Try(s.toInt) match {
          case Success(value) if 1886 to 2100 contains value => Some(ProductionYear(value))
          case _ => None
        }
      }
    }

    /** Car license plate number. */
    final case class LicensePlateNumber(value: String) extends AnyVal

    object LicensePlateNumber {

      /** Constructs [[LicensePlateNumber]] from the string.
        * <p/>
        * The string must contain 3 to 10 upper case letters and numbers.
        */
      def fromString(s: String): Option[LicensePlateNumber] = {
        if (!s.matches("^[A-Z0-9]{3,10}$"))
          None
        else
          Some(LicensePlateNumber(s))
      }
    }

    /** Car engine type. */
    sealed trait EngineType {
      def value: String
    }

    object EngineType {
      final case object Petrol extends EngineType {
        override val value: String = "P"
      }

      final case object Diesel extends EngineType {
        override val value: String = "D"
      }

      final case object Hybrid extends EngineType {
        override val value: String = "H"
      }

      final case object Electric extends EngineType {
        override val value: String = "E"
      }

      /** Constructs [[EngineType]] from the string.
        * <p/>
        * The string must be one of the predefined values:
        * <ul>
        * <li>"p" or "P" for [[Petrol]];</li>
        * <li>"d" or "D" for [[Diesel]];</li>
        * <li>"h" or "H" for [[Hybrid]];</li>
        * <li>"e" or "E" for [[Electric]].</li>
        * </ul>
        */
      def fromString(s: String): Option[EngineType] = s.toLowerCase match {
        case "p" => Some(Petrol)
        case "d" => Some(Diesel)
        case "h" => Some(Hybrid)
        case "e" => Some(Electric)
        case _ => None
      }
    }
  }

  final case class ParsingFailure(lineNumber: LineNumber, reason: Reason)

  object ParsingFailure {

    /** Free-form reason for the parsing failure, must not be blank. */
    final case class Reason(value: String) extends AnyVal
  }

  /** Parsing result. Can be parsedResult a [[ParsingFailure]] or some success value. */
  type ParsingResult[A] = Either[ParsingFailure, A]

  object CarParser {

    /** Tries to parse one CSV data line into a [[Car]]. */
    def parse(line: Line, lineNumber: LineNumber): ParsingResult[Car] = {
      val items = line.value.split(",")

      if (items.size != 5)
        Left(ParsingFailure(lineNumber, Reason("Invalid input")))
      else {
        val maybeManufacturer = Manufacturer.fromString(items(0))
        val maybeModel = Model.fromString(items(1))
        val maybeYear = ProductionYear.fromString(items(2))
        val maybeNumber = LicensePlateNumber.fromString(items(3))
        val maybeType = EngineType.fromString(items(4))

        if (
          maybeManufacturer.isEmpty ||
            maybeModel.isEmpty ||
            maybeYear.isEmpty ||
            maybeNumber.isEmpty ||
            maybeType.isEmpty
        ) Left(ParsingFailure(lineNumber, Reason("Invalid input")))
        else
          Right(Car(maybeManufacturer.get, maybeModel.get, maybeYear.get, maybeNumber.get, maybeType.get))
      }
    }

    /** Tries to parse all CSV data sequentially into a list of [[Car Cars]]. */
    def parseCsv(csv: Csv): List[ParsingResult[Car]] = {
      csv.lines.zipWithIndex.map(line => parse(line._1, LineNumber(line._2)))
    }
  }

  object CarSorter {

    /** Sorts the list of [[Car Cars]] according to the [[SortCriteria]]. */
    def sort(cars: List[Car], sortCriteria: SortCriteria): List[Car] = {

      val sortCarsByCriteria: (Car, Car, SortCriteria) => Boolean = (carLeft, carRight, sortCriteria) => {

        val fields: (String, String) = sortCriteria match {
          case SortCriteria(ByManufacturer, _) => (carLeft.manufacturer.value, carRight.manufacturer.value)
          case SortCriteria(ByModel, _) => (carLeft.model.value, carRight.model.value)
          case SortCriteria(ByProductionYear, _) => (carLeft.productionYear.value.toString, carRight.productionYear.value.toString)
          case SortCriteria(ByLicensePlateNumber, _) => (carLeft.licensePlateNumber.value, carRight.licensePlateNumber.value)
          case SortCriteria(ByEngineType, _) => (carLeft.engineType.value, carRight.engineType.value)
        }

        sortCriteria match {
          case SortCriteria(_, Ascending) => fields._1 < fields._2
          case SortCriteria(_, Descending) => fields._1 > fields._2
        }
      }

      cars.sortWith(sortCarsByCriteria(_, _, sortCriteria))
    }
  }

  object CarProcessor {

    /** Tries to parse CSV data into a list of [[Car Cars]] and sort them.
      * <p/>
      * If there are [[ParsingFailure ParsingFailures]], combines all of them into a list and returns it.
      * In this case the list should be sorted by [[LineNumber]] in ascending order.
      * <p/>
      * If there are no [[ParsingFailure ParsingFailures]], returns back the list of [[Car Cars]], sorted
      * according to the [[SortCriteria]].
      */
    def process(sortCriteria: SortCriteria)(csv: Csv): Either[List[ParsingFailure], List[Car]] = {
      val parsedResult = CarParser.parseCsv(csv).partition(_.isLeft) match {
        case (Nil, rights) => Right(for (Right(i) <- rights) yield i)
        case (lefts, _) => Left(for (Left(s) <- lefts) yield s)
      }

      parsedResult match {
        case Left(value) => Left(value.sortBy(_.lineNumber.value))
        case Right(value) => Right(CarSorter.sort(value, sortCriteria))
      }
    }
  }
}
