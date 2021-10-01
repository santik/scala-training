package com.training.csvparser

import com.training.csvparser.CsvParser._
import com.training.csvparser.CsvParser.Car._
import com.training.csvparser.CsvParser.Csv._
import com.training.csvparser.CsvParser.SortCriteria._
import com.training.csvparser.CsvParserTest._
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CsvParserTest
  extends AnyFreeSpec
    with Matchers
    with EitherValues {

  "Manufacturer should" - {
    "accept valid strings" in {
      Manufacturer.fromString("BMW") should contain(bmw.manufacturer)
      Manufacturer.fromString("Volkswagen") should contain(volkswagen.manufacturer)
    }
    "reject invalid strings" in {
      Manufacturer.fromString("") shouldBe None
    }
  }

  "Model should" - {
    "accept valid strings" in {
      Model.fromString("i3") should contain(bmw.model)
      Model.fromString("Beetle") should contain(volkswagen.model)
    }
    "reject invalid strings" in {
      Model.fromString("") shouldBe None
    }
  }

  "ProductionYear should" - {
    "accept valid strings" in {
      ProductionYear.fromString("2019") should contain(bmw.productionYear)
    }
    "reject invalid strings" in {
      ProductionYear.fromString("ABCD") shouldBe None
    }
  }

  "LicensePlateNumber should" - {
    "accept valid strings" in {
      LicensePlateNumber.fromString("AB1234") should contain(bmw.licensePlateNumber)
    }
    "reject invalid strings" in {
      LicensePlateNumber.fromString("@@@") shouldBe None
    }
  }

  "EngineType should" - {
    "accept valid strings" in {
      EngineType.fromString("P") should contain(EngineType.Petrol)
      EngineType.fromString("D") should contain(EngineType.Diesel)
      EngineType.fromString("H") should contain(EngineType.Hybrid)
      EngineType.fromString("E") should contain(EngineType.Electric)
    }
    "reject invalid strings" in {
      EngineType.fromString("!") shouldBe None
    }
  }

  "CarParser should" - {
    "parse valid lines" in {
      CarParser.parse(
        line = Line("BMW,i3,2019,AB1234,E"),
        lineNumber = LineNumber(1),
      ).value shouldBe bmw
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
      val cars = List(bmw, volkswagen)

      CarSorter.sort(
        cars = cars,
        sortCriteria = SortCriteria(Field.ByManufacturer, Order.Descending),
      ) shouldBe List(volkswagen, bmw)

      CarSorter.sort(
        cars = cars,
        sortCriteria = SortCriteria(Field.ByLicensePlateNumber, Order.Ascending),
      ) shouldBe List(bmw, volkswagen)
    }
  }
}

object CsvParserTest {

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
