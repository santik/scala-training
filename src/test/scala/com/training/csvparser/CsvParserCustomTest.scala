
package com.training.csvparser

import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import com.training.csvparser.CsvParser._
import com.training.csvparser.CsvParser.Car._
import com.training.csvparser.CsvParser.Csv._
import com.training.csvparser.CsvParser.SortCriteria._

import scala.util.Random

class CsvParserCustomTest
  extends AnyFreeSpec
    with Matchers
    with EitherValues {

  "Manufacturer should" - {
    "accept valid strings" in {
      Manufacturer.fromString(Random.nextString(100)).nonEmpty shouldBe true
      Manufacturer.fromString("  1  ").get.value shouldBe "  1  "
    }
    "reject invalid strings" in {
      Manufacturer.fromString("1") shouldBe None
      Manufacturer.fromString("    ") shouldBe None
      Manufacturer.fromString(Random.nextString(101)) shouldBe None
    }
  }

  "Model should" - {
    "accept valid strings" in {
      Model.fromString(Random.nextString(200)).nonEmpty shouldBe true
      Model.fromString("  1  ").get.value shouldBe "  1  "
      Model.fromString("1").get.value shouldBe "1"
    }
    "reject invalid strings" in {
      Model.fromString(Random.nextString(201)) shouldBe None
    }
  }

  "ProductionYear should" - {
    "accept valid strings" in {
      ProductionYear.fromString("1886").get.value shouldBe 1886
      ProductionYear.fromString("2100").get.value shouldBe 2100
    }
    "reject invalid strings" in {
      ProductionYear.fromString("ABCD") shouldBe None
      ProductionYear.fromString("ABC1") shouldBe None
      ProductionYear.fromString("1BC1") shouldBe None
      ProductionYear.fromString("1885") shouldBe None
      ProductionYear.fromString("2101") shouldBe None
    }
  }

  "LicensePlateNumber should" - {
    "reject invalid strings" in {
      LicensePlateNumber.fromString("AB") shouldBe None
      LicensePlateNumber.fromString("ABABABABABA") shouldBe None
      LicensePlateNumber.fromString("aaa") shouldBe None
      LicensePlateNumber.fromString("AAA$") shouldBe None
    }
  }

  "EngineType should" - {
    "accept valid strings" in {
      EngineType.fromString("p") should contain(EngineType.Petrol)
      EngineType.fromString("d") should contain(EngineType.Diesel)
      EngineType.fromString("h") should contain(EngineType.Hybrid)
      EngineType.fromString("e") should contain(EngineType.Electric)
    }
    "reject invalid strings" in {
      EngineType.fromString(" ") shouldBe None
    }
  }

  "CarParser should" - {
    "parse valid lines" in {
      CarParser.parse(
        line = Line("BMW,i3,2019,AB1234,E"),
        lineNumber = LineNumber(1),
      ).value shouldBe CsvParserCustomTest.bmw
    }
    "report failures for invalid lines" in {
      CarParser.parse(
        line = Line("invalid"),
        lineNumber = LineNumber(1),
      ).left.value shouldBe a[ParsingFailure]
    }
  }

  "CarSorter should" - {
    "sort as expected" in {
      val cars = List(CsvParserCustomTest.bmw, CsvParserCustomTest.volkswagen)

      CarSorter.sort(
        cars = cars,
        sortCriteria = SortCriteria(Field.ByModel, Order.Ascending),
      ) shouldBe List(CsvParserCustomTest.volkswagen, CsvParserCustomTest.bmw)

      CarSorter.sort(
        cars = cars,
        sortCriteria = SortCriteria(Field.ByEngineType, Order.Ascending),
      ) shouldBe List(CsvParserCustomTest.bmw, CsvParserCustomTest.volkswagen)
    }
  }

  "Processor should" - {
    "process incorrect csv as expected" in {
      val csv: Csv = Csv(List(
        Line(""),
        Line("")
      ))
      val result = CarProcessor.process(SortCriteria(Field.ByModel, Order.Ascending))(csv)
      result.isLeft shouldBe true
      result.left.value shouldBe a [List[_]]
      result.left.value should have size 2
      result.left.value(0) shouldBe a [ParsingFailure]
      result.left.value(1) shouldBe a [ParsingFailure]
    }
    "process correct csv as expected" in {

      val csv: Csv = Csv(List(
        Line("BMW,i3,2019,AB1234,E"),
        Line("ASL,i3,2019,AB1234,E")
      ))
      val result = CarProcessor.process(SortCriteria(Field.ByManufacturer, Order.Ascending))(csv)
      result.isRight shouldBe true
      result.value shouldBe a [List[_]]
      result.value should have size 2
      result.value(0) shouldBe a [Car]
      result.value(1) shouldBe a [Car]
      result.value(0).manufacturer.value shouldBe "ASL"
    }

    "process mixed csv as expected" in {
      val csv: Csv = Csv(List(
        Line("BMW,i3,2019,aa1234,E"),
        Line("BMW,i3,2019,AB1234,E"),
        Line("ASL,i3,20190,AB1234,E")
      ))
      val result = CarProcessor.process(SortCriteria(Field.ByManufacturer, Order.Ascending))(csv)
      result.isLeft shouldBe true
      result.left.value shouldBe a [List[_]]
      result.left.value should have size 2
      result.left.value(0).lineNumber.value shouldBe 0
      result.left.value(1).lineNumber.value shouldBe 2
    }
  }
}

object CsvParserCustomTest {

  private val bmw = Car(
    manufacturer = Manufacturer("BMW"),
    model = Model("i3"),
    productionYear = ProductionYear(2019),
    licensePlateNumber = LicensePlateNumber("AB1234"),
    engineType = EngineType.Electric,
  )
  private val volkswagen = Car(
    manufacturer = Manufacturer("Volkswagen"),
    model = Model("Beetle"),
    productionYear = ProductionYear(2011),
    licensePlateNumber = LicensePlateNumber("DE5432"),
    engineType = EngineType.Petrol,
  )
}

