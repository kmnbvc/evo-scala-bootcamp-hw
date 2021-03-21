package cats

import cats.data.NonEmptyList
import cats.implicits._

trait MonoidHierarchy {

  /*
    Semigroup[A] {
      def combine(y: A): A
    }
   */

  /*
    Monoid[A] {
      def combine(y: A): A
    }

    def empty: A
   */

  // NonEmptyList

  type Error = String
  type Result[T] = Either[NonEmptyList[Error], T]

  // laws
  type A
  val anyValue: A = ???
  val v1: A = ???
  val v2: A = ???
  val v3: A = ???

  implicit val instance: Monoid[A]

  (Monoid[A].empty |+| anyValue) == anyValue // left identity

  (anyValue |+| Monoid[A].empty) == anyValue // right identity

  ((v1 |+| v2) |+| v3) == (v1 |+| (v2 |+| v3)) // associativity
}

object MonadHierarchy {
  /*
    Functor[F[A]] {
      def map(f: A => B): F[B]
    }
   */

  /*
    Applicative[F[A]] {
      def ap(f: F[A => B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  /*
    Monad[F[A]] {
      def flatMap(f: A => F[B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  // applicative: (F[A], F[B]) => F[(A, B)]
  // monad:        F[F[A]]     => F[A]
}

object Excercises {

  trait Applicative[F[_]] extends Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      apply(map(fa)(f.curried))(fb)
    }

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
      case head :: tail => map2(head, sequence(tail))(_ :: _)
      case Nil => unit(Nil)
    }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => unit(f(x)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(f(a, _)))
  }

}
