package com.training.typeclasses

import com.training.typeclasses.TypeClassesHomework.Foldable.Tree.{Branch, Leaf}

import scala.annotation.tailrec

/**
 * Try to accomplish as many tasks as you can
 */
object TypeClassesHomework {

  object OrderingTask {

    final case class Money(amount: BigDecimal)

    // TODO Implement Ordering instance for Money
    implicit val moneyOrdering: Ordering[Money] = new Ordering[Money] {
      override def compare(x: Money, y: Money): Int = x.amount.compareTo(y.amount)
    }
  }

  object ShowTask {

    trait Show[T] { // Fancy toString
      def show(entity: T): String
    }

    object Show {
      def apply[A](implicit instance: Show[A]):Show[A] = instance
    }

    implicit class ShowOps[T](x: T) {
      def show(implicit show: Show[T]): String = show.show(x)
    }

    implicit val stringShow: Show[User] = (u: User) => s"id: ${u.id} name: ${u.name}"

    final case class User(id: String, name: String)

    // TODO Implement Show instance for User
    val showUser: Show[User] = new Show[User] {
      override def show(entity: User): String = s"id: ${entity.id} name: ${entity.name}"
    }

    // TODO Implement syntax for Show so I can do User("1", "John").show
    User("1", "John").show
  }

  object ParseTask extends App {

    type Error = String

    trait Parse[T] { // Feel free to use any format. It could be CSV or anything else.
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    // TODO Implement Parse instance for User
    val userParse: Parse[User] = new Parse[User] {
      override def parse(entity: String): Either[Error, User] = {
        val parts = entity.split(",")
        if (parts.length != 2)
          Left(new Error("Invalid input"))
        else
          Right(User(parts(0), parts(1)))
      }
    }

    // TODO Implement syntax for Parse so I can do "lalala".parse[User] (and get an error because it is obviously not a User)
    object Parse {
      def apply[A](implicit instance: Parse[A]):Parse[A] = instance
    }

    implicit class ParseOps[T](x: String) {
      def parse[T](implicit parse: Parse[T]): Either[Error, T] = parse.parse(x)
    }

    implicit val stringParseToUser: Parse[User] = (s: String) => userParse.parse(s)

    println("lalala".parse[User])

  }

  object EqualsTask {
    // TODO Design a typesafe equals so I can do a === b, but it won't compile if a and b are of different types
    // Define the typeclass (think of a method signature)
    // Keep in mind that `a method b` is `a.method(b)`

    trait Eq[T] {
      def ===(a: T)(b: T): Boolean
    }

    object Eq {
      def apply[A](b: A)(implicit instance: Eq[A]):Eq[A] = instance
    }

    implicit class EqOps[T](a: T) {
      def ===(b: T)(implicit eq: Eq[T]): Boolean = eq.===(a)(b)
    }

    final case class User(id: String, name: String)
    final case class AnotherUser(id: String, name: String)


    implicit val userEq: Eq[User] = new Eq[User] {
      override def ===(a: User)(b: User): Boolean = a.id == b.id
    }

    User("1", "a") === User("2", "b")
//    User("1", "a") === AnotherUser("2", "b")
  }

  object Foldable {

    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    trait Foldable[F[_]] {
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
      def foldMap[A, B](as: F[A])(f: A => B)(implicit monoid: Monoid[B]): B
    }

    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
        as match {
          case Some(value) => f(z, value)
          case None => z
        }
      }

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
        as match {
          case Some(value) => f (value, z)
          case None => z
        }
      }

      override def foldMap[A, B](as: Option[A])(f: A => B)(implicit monoid: Monoid[B]): B = {
        as match {
          case Some(value) => f(value)
          case None => monoid.empty
        }
      }
    } // TODO Implement Foldable instance for Option

    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
        @tailrec
        def walk(as: List[A])(z: B): B = {
          if (as.isEmpty) z
          else walk(as.tail)(f(z, as.head))
        }

        walk(as)(z)
      }

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
        @tailrec
        def walk(as: List[A])(z: B): B = {
          if (as.isEmpty) z
          else walk(as.takeRight(as.length - 1))(f(as.last, z))
        }

        walk(as)(z)
      }

      override def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B = {
        @tailrec
        def walk(as: List[A])(z:B): B = {
          if (as.isEmpty) monoid.empty
          else walk(as.tail)(monoid.combine(f(as.head), z))
        }

        walk(as)(monoid.empty)
      }
    } // TODO Implement Foldable instance for List

    sealed trait Tree[A]
    object Tree {
      final case class Leaf[A](value: A) extends Tree[A]
      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    }

    implicit val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
        def walk(as: Tree[A])(z:B): B =
          as match {
            case Leaf(value) => f(z, value)
            case Branch(left, right) => walk(left)(walk(right)(z))
          }

        walk(as)(z)
      }


      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
        def walk(as: Tree[A])(z:B): B =
          as match {
            case Leaf(value) => f(value, z)
            case Branch(left, right) => walk(right)(walk(left)(z))
          }

        walk(as)(z)
      }

      override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit monoid: Monoid[B]): B = {
        def walk(as: Tree[A])(z:B): B =
          as match {
            case Leaf(value) => monoid.combine(f(value), z)
            case Branch(left, right) => walk(left)(monoid.combine(z, walk(right)(z)))
          }

        walk(as)(monoid.empty)
      }
    } // TODO Implement Foldable instance for Tree
  }

  object ApplicativeTask {

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] with Semigroupal[F] {

      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] // "ap" here stands for "apply" but it's better to avoid using it

      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
        val func: A => B => (A, B) = a => b => (a, b)
        ap(map(fa)(func))(fb)
      } // TODO Implement using `ap` and `map`

      def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = {
        val func: ((A, B)) => Z = ab => f(ab._1, ab._2)
        map(product(fa, fb))(func)
      } // TODO Implement using `map` and `product`
    }

    trait Applicative[F[_]] extends Apply[F] {
      def pure[A](a: A): F[A]
    }

    // TODO Implement Applicative instantce for Option
    implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
      override def pure[A](a: A): Option[A] = Some(a)

      override def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = {
        (fab, fa) match {
          case (Some(func), Some(value)) => Some(func(value))
          case _ => None
        }
      }
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
        fa.flatMap(a => Some(f(a)))
      }
    } // Keep in mind that Option has flatMap

    // TODO Implement traverse using `map2`
    // I didn't get how to use map2 here
    def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

    // TODO Implement sequence (ideally using already defined things)
    def sequence[F[_]: Applicative, A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(fa => fa)
  }
}