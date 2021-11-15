package com.training.iomonad

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Left, Right, Success, Try}

/*
 * Provide your own implementation of a subset of `IO` functionality.
 *
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/docs/2.x/datatypes/io
 *  - https://typelevel.org/cats-effect/api/2.x/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/2.x/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object IOMonadTask {

  final class IO[A] (private val run: () => A) {

    def map[B](f: A => B): IO[B] = {
      new IO[B](() => f(run()))
    }
    def flatMap[B](f: A => IO[B]): IO[B] =  {
      f(run())
    }

    def *>[B](another: IO[B]): IO[B] = another

    def as[B](newValue: => B): IO[B] = IO {
      run()
      newValue
    }

    def void: IO[Unit] = new IO(() => run())

    def attempt: IO[Either[Throwable, A]] = IO {
      Try(run()).toEither
    }

    def option: IO[Option[A]] = IO {
      Try(run()).toOption
    }

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      Try(run()) match {
        case Failure(exception) => f (exception)
        case Success(value) => IO(value)
      }

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO {
      Try(run()) match {
        case Failure(exception) => recover(exception)
        case Success(value) => map(value)
      }
    }

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      Try(run()) match {
        case Failure(exception) => recover(exception)
        case Success(value) => bind(value)
      }

    def unsafeRunSync(): A = run()

    def unsafeToFuture(): Future[A] = {
      val promise = Promise[A]()
      promise.tryComplete(Try(run()))
      promise.future
    }
  }

  object IO {

    def apply[A](body: => A): IO[A] = new IO[A](() => body)

    def suspend[A](thunk: => IO[A]): IO[A] = IO(thunk.run())

    def delay[A](body: => A): IO[A] = IO{body}

    def pure[A](a: A): IO[A] = IO{a}

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(a)  => pure(a)
      case Left(err) => raiseError(err)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None  => raiseError(orElse)
      case Some(value) => IO{value}
    }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(value) => IO{value}
      case Failure(e) => raiseError(e)
    }

    def none[A]: IO[Option[A]] = pure(None)

    def raiseError[A](e: Throwable): IO[A] = IO {
      throw e
    }

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = IO {
      if (!cond) {
        throw e
      }
    }

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = IO {
      if (cond) {
        throw e
      }
    }

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = IO {
      if (!cond) {
        action.unsafeRunSync()
      }
    }

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = IO {
      if (cond) {
        action.unsafeRunSync()
      }
    }

    val unit: IO[Unit] = new IO(() => none)
  }
}
