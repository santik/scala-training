package com.training.cats

import cats.data.{NonEmptyList, NonEmptySet, ValidatedNel}
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.kernel.Semigroup
import cats.syntax.all._
import cats.{Id, Monad}
import org.scalatest.Inspectors
import org.scalatest.freespec.AnyFreeSpec

import java.time.YearMonth
import java.util.UUID
import scala.util.Random

class CatsHomeworkTest extends AnyFreeSpec with Inspectors {
  "MonoidUsage" - {
    import CatsHomework.MonoidUsage._

    "ClientReport semigroup" in {
      val a = ClientReport(
        total = 10,
        lastProblemId = UUID.randomUUID(),
        kinds = NonEmptySet.of("problem1", "problem2"),
        totalPerKind = Map("problem1" -> 5, "problem2" -> 5),
      )
      val b = ClientReport(
        total = 8,
        lastProblemId = UUID.randomUUID(),
        kinds = NonEmptySet.of("problem1", "problem3"),
        totalPerKind = Map("problem1" -> 2, "problem3" -> 6),
      )

      val result = Semigroup[ClientReport].combine(a, b)
      val expected = ClientReport(
        total = 18,
        lastProblemId = b.lastProblemId,
        kinds = NonEmptySet.of("problem1", "problem2", "problem3"),
        totalPerKind = Map("problem1" -> 7, "problem2" -> 5, "problem3" -> 6),
      )

      assert(result == expected)
    }

    "Per-client aggregation" in {
      val problems = Random.shuffle {
        for {
          kind <- Vector("it_broke", "works_for_me", "out_of_magic_smoke")
          client <- Vector("important", "maybe_important", "probably_important")
          amount = Random.between(1, 20)
          problem <- Vector.fill(amount)(Problem(kind, client, UUID.randomUUID()))
        } yield problem
      }

      val aggregated = aggregate(problems)

      assert(aggregated.keySet == problems.view.map(_.client).toSet)

      forEvery(aggregated) { case (client, report) =>
        val problemsForClient = problems.filter(_.client == client)
        assert(report.total == problemsForClient.size)
        assert(report.lastProblemId == problemsForClient.last.id)
        assert(report.totalPerKind.values.sum == report.total)
      }
    }
  }

  "PolyValidation" - {
    import CatsHomework.PolyValidation._

    type ErrorsOr[T] = ValidatedNel[ValidationError, T]

    // Valid values, for convenience
    val number = "1234567890123456"
    val exp = YearMonth.of(2022, 12)
    val name = "John Cardholder"
    val code = "123"

    "invalid values" in {
      assert(validate[ErrorsOr](number, exp, name, code).isValid)

      assert(validate[ErrorsOr]("", exp, name, code).isInvalid)
      assert(validate[ErrorsOr]("123456789012345", exp, name, code).isInvalid)
      assert(validate[ErrorsOr]("12345678901234567", exp, name, code).isInvalid)
      assert(validate[ErrorsOr]("1234567890123abc", exp, name, code).isInvalid)

      assert(validate[ErrorsOr](number, exp, "", code).isInvalid)
      assert(validate[ErrorsOr](number, exp, "Robert'); DROP TABLE Students;--", code).isInvalid)

      assert(validate[ErrorsOr](number, exp, name, "").isInvalid)
      assert(validate[ErrorsOr](number, exp, name, "12").isInvalid)
      assert(validate[ErrorsOr](number, exp, name, "1234").isInvalid)
      assert(validate[ErrorsOr](number, exp, name, "12a").isInvalid)
    }

    "error types and accumulation" in {
      assert(validate[ErrorsOr]("", exp, name, code) == InvalidNumber.invalidNel)
      assert(validate[ErrorsOr](number, exp, "", code) == InvalidName.invalidNel)
      assert(validate[ErrorsOr](number, exp, name, "") == InvalidSecurityCode.invalidNel)

      assertResult(NonEmptyList.of(InvalidNumber, InvalidName, InvalidSecurityCode).invalid) {
        validate[ErrorsOr]("", exp, "", "")
      }
    }
  }

  "MonadTransformers" - {
    import CatsHomework.MonadTransformers._

    // Helper for convenient request construction, Option(null) == None
    def mkRequest(op: String, key: String = null, value: String = null, prev: String = null) =
      RawRequest(op = op, key = Option(key), value = Option(value), previousValue = Option(prev))

    "parseRequest" in {
      // Helper for convenient request construction, Option(null) == None
      def parse(op: String, key: String = null, value: String = null, prev: String = null) =
        parseRequest(mkRequest(op, key, value, prev))

      assert(parse("write", key = "foo", value = "bar") contains Request.Write("foo", "bar"))
      assert(parse("write", key = "foo").isEmpty)
      assert(parse("write", value = "bar").isEmpty)
      assert(parse("write").isEmpty)

      assert(parse("read", key = "foo") contains Request.Read("foo"))
      assert(parse("read").isEmpty)

      assert(
        parse("cas", key = "foo", value = "bar", prev = "baz")
          .contains(Request.CompareAndSet(key = "foo", expected = "baz", newValue = "bar"))
      )
      assert(parse("cas", key = "foo", value = "bar").isEmpty)
      assert(parse("cas", key = "foo").isEmpty)
      assert(parse("cas").isEmpty)

      assert(parse("read-unref", key = "foo") contains Request.ReadUnref("foo"))
      assert(parse("read-unref").isEmpty)

      assert(parse("bogus").isEmpty)
      assert(parse("bogus", key = "foo").isEmpty)
      assert(parse("bogus", key = "foo", value = "bar").isEmpty)
    }

    // Test database, data is a public field for tests
    class TestDb(var data: Map[Key, Value]) extends KVDatabase[Id] {
      override def write(key: Key, value: Value): Unit = data += key -> value
      override def read(key: Key): Option[Value] = data.get(key)
    }
    def mkDb(data: Map[Key, Value]) = new TestDb(data)

    "executeRequest" in {
      import Response._
      // Helper to run a request and return database contents afterwards and response
      def run(data: Map[Key, Value], request: Request): (Map[Key, Value], Option[Response]) = {
        val db = new TestDb(data)
        val result = executeRequest[Id](db, request)
        db.data -> result.value
      }

      assert(run(Map(), Request.Write("foo", "bar")) == Map("foo" -> "bar") -> Some(Ok))
      assert(run(Map("foo" -> "bar"), Request.Write("foo", "baz")) == Map("foo" -> "baz") -> Some(Ok))

      assert(run(Map("foo" -> "bar"), Request.Read("foo")) == Map("foo" -> "bar") -> Some(Found("bar")))
      assert(run(Map("foo" -> "bar"), Request.Read("missing")) == Map("foo" -> "bar") -> None)

      assert(run(Map("foo" -> "bar"), Request.CompareAndSet("foo", "bar", "baz")) == Map("foo" -> "baz") -> Some(Ok))
      assert(run(Map("foo" -> "bar"), Request.CompareAndSet("foo", "other", "bar")) == Map("foo" -> "bar") -> None)
      assert(run(Map("foo" -> "bar"), Request.CompareAndSet("missing", "bar", "bar")) == Map("foo" -> "bar") -> None)

      val unrefData = Map("a" -> "b", "b" -> "c", "c" -> "not this")
      assert(run(unrefData, Request.ReadUnref("a")) == unrefData -> Some(Found("c")))
      assert(run(unrefData, Request.ReadUnref("c")) == unrefData -> None)
      assert(run(unrefData, Request.ReadUnref("missing")) == unrefData -> None)
    }

    "runReq" in {
      def runProgram[F[_]: Monad](db: KVDatabase[F]) = {
        for {
          _ <- runReq(db, mkRequest("write", "a", "b"))
          _ <- runReq(db, mkRequest("write", "b", "c"))
          _ <- runReq(db, mkRequest("write", "foo", "bar"))
          b <- runReq(db, mkRequest("read", "a"))
          _ <- runReq(db, mkRequest("cas", "a", "foo", prev = "b"))
          bar <- runReq(db, mkRequest("read-unref", "a"))
        } yield Seq(b, bar)
      }

      val expectedResult = Seq(Response.Found("b").some, Response.Found("bar").some)

      assert(runProgram(new InMemoryStubDb()) == expectedResult)

      // This program should give the same result when run against something more production-like
      class IODb(data: Ref[IO, Map[Key, Value]]) extends KVDatabase[IO] {
        override def write(key: Key, value: Value): IO[Unit] = data.update(_.updated(key, value))
        override def read(key: Key): IO[Option[Value]] = data.get.map(_.get(key))
      }
      object IODb {
        def apply(): IO[IODb] = Ref[IO].of(Map.empty[Key, Value]).map(new IODb(_))
      }

      val ioResult = IODb().flatMap(runProgram(_))
      assert(ioResult.unsafeRunSync() == expectedResult)
    }
  }
}
