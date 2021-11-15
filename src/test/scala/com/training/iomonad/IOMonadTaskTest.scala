package com.training.iomonad

import com.training.iomonad.IOMonadTask.IO
import junit.framework.TestCase
import org.junit.Assert.assertEquals

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

class IOMonadTaskTest extends TestCase {

  def testUnsafeRunSync() {
    assertEquals(42, IO.pure(42).unsafeRunSync())
  }

  def testMap() {
    assertEquals(25, IO.pure(5).map(_ * 5).unsafeRunSync())
    assertEquals(5, IO.pure(5).map(identity).unsafeRunSync())
    assertEquals(100, IO.pure(5).map(_ => 100).unsafeRunSync())
    assertEquals("42", IO.pure(42).map(_.toString).unsafeRunSync())
  }

  def testFlatMap() {
    var i      = 0
    val result = (for {
      a  <- IO.pure(10)
      b  <- IO.pure(20)
      c  <- IO.pure(30)
      sum = a + b + c
      _  <- IO { i = sum }
    } yield sum).unsafeRunSync()
    assertEquals(60, result)
    assertEquals(60, i)
  }

  def `test*>`() {
    assertEquals(1, (IO.pure(100) *> IO.pure(1)).unsafeRunSync())
  }

  def testAs() {
    assertEquals(1, IO.pure(100).as(1).unsafeRunSync())
    assertEquals("result", IO.pure(100).as("result").unsafeRunSync())
  }

  def testVoid() {
    var i            = 0
    val result: Unit = IO { i = 10; i }.void.unsafeRunSync()
    assertEquals((), result)
    assertEquals(10, i)
  }

  def testAttempt() {
    val ex = new RuntimeException("error")
    assertEquals(Right(42), IO.pure(42).attempt.unsafeRunSync())
    assertEquals(Left(ex), IO(throw ex).attempt.unsafeRunSync())
  }

  def testOption() {
    assertEquals(Some(42), IO.pure(42).option.unsafeRunSync())
    assertEquals(None, IO(throw new RuntimeException("error")).option.unsafeRunSync())
  }

  def testHandleErrorWith() {
    assertEquals(42, IO.pure(42).handleErrorWith(_ => IO.pure(24)).unsafeRunSync())
    assertEquals(24, IO(throw new RuntimeException("error")).handleErrorWith(_ => IO.pure(24)).unsafeRunSync())
  }

  def testRedeem() {
    assertEquals(42, IO.pure(42).redeem(_ => 24, identity).unsafeRunSync())
    assertEquals(24, (IO.pure(42) *> IO(throw new RuntimeException("error"))).redeem(_ => 24, identity).unsafeRunSync())
  }

  def testRedeemWith() {
    assertEquals(42, IO.pure(42).redeemWith(_ => IO.pure(24), IO.pure).unsafeRunSync())
    assertEquals(
      24,
      (IO.pure(42) *> IO[Int](throw new RuntimeException("error")))
        .redeemWith(_ => IO.pure(24), IO.pure)
        .unsafeRunSync()
    )
  }

  def testUnsafeToFuture() {
    assertEquals(42, Await.result(IO.pure(42).unsafeToFuture(), Duration.Inf))
  }

  def testApply() {
    var i      = 0
    val result = IO { i = 42; i }.unsafeRunSync()
    assertEquals(42, result)
    assertEquals(42, i)
  }

  def testSuspend(): Unit =
    assertEquals(42, IO.suspend(IO.pure(42)).unsafeRunSync())

  def testDelay(): Unit = {
    var i      = 0
    val result = IO.delay { i = 42; i }.unsafeRunSync()
    assertEquals(42, result)
    assertEquals(42, i)
  }

  def testPure(): Unit =
    assertEquals(42, IO.pure(42).unsafeRunSync())

  def testFromEither(): Unit = {
    assertEquals(
      24,
      IO.fromEither(Left(new RuntimeException("error"))).handleErrorWith(_ => IO.pure(24)).unsafeRunSync()
    )
    assertEquals(42, IO.fromEither(Right(42)).unsafeRunSync())
  }

  def testFromOption(): Unit = {
    assertEquals(
      24,
      IO.fromOption(None)(new RuntimeException("error")).handleErrorWith(_ => IO.pure(24)).unsafeRunSync()
    )
    assertEquals(42, IO.fromOption(Some(42))(new RuntimeException("error")).unsafeRunSync())
  }

  def testFromTry(): Unit = {
    assertEquals(
      24,
      IO.fromTry(Try(throw new RuntimeException("error"))).handleErrorWith(_ => IO.pure(24)).unsafeRunSync()
    )
    assertEquals(42, IO.fromTry(Try(42)).unsafeRunSync())
  }

  def testNone(): Unit =
    assertEquals(None, IO.none.unsafeRunSync())

  def testRaiseError(): Unit =
    assertEquals(24, IO.raiseError(new RuntimeException("error")).handleErrorWith(_ => IO.pure(24)).unsafeRunSync())

  def testRaiseUnless(): Unit = {
    assertEquals(42, IO.raiseUnless(cond = true)(new RuntimeException("error")).as(42).unsafeRunSync())
    assertEquals(
      24,
      IO.raiseUnless(cond = false)(new RuntimeException("error"))
        .as(42)
        .handleErrorWith(_ => IO.pure(24))
        .unsafeRunSync()
    )
  }

  def testRaiseWhen(): Unit = {
    assertEquals(42, IO.raiseWhen(cond = false)(new RuntimeException("error")).as(42).unsafeRunSync())
    assertEquals(
      24,
      IO.raiseWhen(cond = true)(new RuntimeException("error"))
        .as(42)
        .handleErrorWith(_ => IO.pure(24))
        .unsafeRunSync()
    )
  }

  def testUnlessA(): Unit = {
    var i = 0
    IO.unlessA(cond = true)(IO { i = 42 }).unsafeRunSync()
    assertEquals(0, i)
    IO.unlessA(cond = false)(IO { i = 42 }).unsafeRunSync()
    assertEquals(42, i)
  }

  def testWhenA(): Unit = {
    var i = 0
    IO.whenA(cond = false)(IO { i = 42 }).unsafeRunSync()
    assertEquals(0, i)
    IO.whenA(cond = true)(IO { i = 42 }).unsafeRunSync()
    assertEquals(42, i)
  }

  def testUnit(): Unit =
    assertEquals((), IO.unit.unsafeRunSync())
}
