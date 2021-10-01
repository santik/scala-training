package com.training.clicalculator

import com.training.clicalculator.CliCalculator.Command._
import com.training.clicalculator.CliCalculator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._

class CliCalculatorTest extends AnyFreeSpec {
  "parseCommand" - {

    "should parse command correctly" - {
      "for input 'divide 4 5'" in {
        parseCommand("divide 4 5").value shouldBe Divide(4, 5)
      }

      "for input 'sum 5 5 6 8.5'" in {
        parseCommand("sum 5 5 6 8.5").value shouldBe Sum(List(5, 5, 6, 8.5))
      }

      "for input 'average 4 3 8.5 4'" in {
        parseCommand("average 4 3 8.5 4").value shouldBe Average(List(4, 3, 8.5, 4))
      }

      "for input 'min 4 -3 -17'" in {
        parseCommand("min 4 -3 -17").value shouldBe Min(List(4, -3, -17))
      }

      "for input 'max 4 -3 -17'" in {
        parseCommand("max 4 -3 -17").value shouldBe Max(List(4, -3, -17))
      }
    }

    "should return Left for input 'invalid command foobar'" in {
      parseCommand("invalid command foobar").isLeft shouldBe true
    }
  }

  "process" - {
    "should process commands correctly" - {
      "for input 'divide 4 5'" in {
        process("divide 4 5") should (
          be("4 divided by 5 is 0.8") or
          be("4.0 divided by 5.0 is 0.8")
        )
      }

      "for input 'sum 5 5 6 8.5'" in {
        process("sum 5 5 6 8.5") should (
          be("the sum of 5 5 6 8.5 is 24.5") or
          be("the sum of 5.0 5.0 6.0 8.5 is 24.5")
        )
      }

      "for input 'average 4 3 8.5 4'" in {
        process("average 4 3 8.5 4") should (
          be("the average of 4 3 8.5 4 is 4.875") or
          be("the average of 4.0 3.0 8.5 4.0 is 4.875")
        )
      }

      "for input 'min 4 -3 -17'" in {
        process("min 4 -3 -17") should (
          be("the minimum of 4 -3 -17 is -17") or
          be("the minimum of 4.0 -3.0 -17.0 is -17.0")
        )
      }

      "for input 'max 4 -3 -17'" in {
        process("max 4 -3 -17") should (
          be("the maximum of 4 -3 -17 is 4") or
          be("the maximum of 4.0 -3.0 -17.0 is 4.0")
        )
      }
    }

    "should return string starting with 'Error: '" - {
      "for input 'invalid command foobar'" in {
        process("invalid command foobar") should startWith("Error: ")
      }
    }
  }
}
