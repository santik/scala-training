package com.training.clicalculator

import scala.io.Source
import scala.util.Try

object CliCalculator {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command {
    def execute : Either[ErrorMessage, Result]
  }
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def execute: Either[ErrorMessage, Result] = {
        if (divisor == 0)
          Left(ErrorMessage("Cannot divide by 0"))
        else
          Right(CommandResult(s"$dividend divided by $divisor is ${dividend / divisor}"))
      }
    }
    final case class Sum(numbers: List[Double]) extends Command {
      override def execute: Either[ErrorMessage, Result] = {
        Right(CommandResult(s"the sum of ${numbers.mkString(" ")} is ${numbers.sum}"))
      }
    }
    final case class Average(numbers: List[Double]) extends Command {
      override def execute: Either[ErrorMessage, Result] = {
        Right(CommandResult(s"the average of ${numbers.mkString(" ")} is ${numbers.sum/numbers.size}"))
      }
    }
    final case class Min(numbers: List[Double]) extends Command {
      override def execute: Either[ErrorMessage, Result] = {
        Right(CommandResult(s"the minimum of ${numbers.mkString(" ")} is ${numbers.min}"))
      }
    }
    final case class Max(numbers: List[Double]) extends Command {
      override def execute: Either[ErrorMessage, Result] = {
        Right(CommandResult(s"the maximum of ${numbers.mkString(" ")} is ${numbers.max}"))
      }
    }
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result
  final case class CommandResult(value: String) extends Result {
    override def toString: String = value
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    val strings = List.from(x.trim.replace(" +", " ").trim.split(" "))

    val args = strings.slice(1, strings.size)
    val numbers = args.map(item => Try(item.toDouble))
      .filter(maybeDouble => maybeDouble.isSuccess)
      .map(maybeDouble => maybeDouble.get)

    if (numbers.isEmpty || numbers.size != args.size)
      Left(ErrorMessage(s"Incorrect input $x"))
    else
      strings match {
        case command :: _ if command == "divide" && numbers.size > 2 =>  Left(ErrorMessage("Divide can have only 2 arguments"))
        case command :: _ if command == "divide" && numbers.size == 2 => Right(Command.Divide(numbers.head, numbers(1)))
        case command :: _ if command == "sum" => Right(Command.Sum(numbers))
        case command :: _ if command == "average" => Right(Command.Average(numbers))
        case command :: _ if command == "min" => Right(Command.Min(numbers))
        case command :: _ if command == "max" => Right(Command.Max(numbers))
        case command :: _ => Left(ErrorMessage(s"Unknown command $command"))
      }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x.execute
  }

  def renderResult(x: Result): String = {
    x.toString
  }

  def process(x: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension
    parseCommand(x) match {
      case Left(value) => s"Error: $value"
      case Right(value) => calculate(value) match {
        case Left(value) => s"Error: $value"
        case Right(value) => renderResult(value)
      }
    }

  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
