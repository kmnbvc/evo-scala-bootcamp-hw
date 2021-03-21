package cats

import scala.Function.const

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(value) => f(value)
      case None => None
    }
    override def pure[A](x: A): Option[A] = Option(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {
    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }
    override def pure[A](x: A): Either[T, A] = Right(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
  }

  def functionMonad[T]: Monad[T => *] = new Monad[T => *] {
    override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = x => f(fa(x))(x)
    override def pure[A](x: A): T => A = const(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
  }
}
