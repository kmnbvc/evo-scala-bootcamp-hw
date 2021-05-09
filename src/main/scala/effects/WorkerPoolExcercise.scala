package effects

import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.implicits._

import scala.concurrent.duration._
import scala.util.Random

object WorkerPoolExcercise extends IOApp {
  // To start, our requests can be modelled as simple functions.
  // You might want to replace this type with a class if you go for bonuses. Or not.
  type Worker[A, B] = A => IO[B]

  // Sample stateful worker that keeps count of requests it has accepted
  def mkWorker(id: Int)(implicit timer: Timer[IO]): IO[Worker[Int, Int]] =
    Ref[IO].of(0).map { counter =>
      def simulateWork: IO[Unit] =
        IO(println(s"work started by $id")) >> IO(50 + Random.nextInt(450) + (if (id == 0) 9999 else 0)).map(_.millis).flatMap(IO.sleep)

      def report: IO[Unit] =
        counter.get.flatMap(i => IO(println(s"Total processed by $id: $i")))

      x =>
        simulateWork >>
          counter.update(_ + 1) >>
          report >>
          IO.pure(x + 1)
    }

  trait WorkerPool[A, B] {
    def exec(a: A): IO[B]
  }

  object WorkerPool {

    // Implement this constructor, and, correspondingly, the interface above.
    // You are free to use named or anonymous classes
    def of[A, B](fs: List[Worker[A, B]]): IO[WorkerPool[A, B]] = {
      MVar.of[IO, List[Worker[A, B]]](fs).map { mvar =>
        a =>
          mvar.modify {
            case head :: tail => IO(tail, Some(head))
            case Nil => IO(Nil, None)
          }.untilDefinedM.bracket(_.apply(a)) {
            worker => mvar.modify_(workers => IO(workers :+ worker))
          }
      }
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    val testPool: IO[WorkerPool[Int, Int]] =
      List.range(0, 10)
        .traverse(mkWorker)
        .flatMap(WorkerPool.of)

    for {
      pool <- testPool
      nums = (0 to 100).toList
      fibers <- nums.traverse(num => pool.exec(num).start)
      result <- fibers.traverse(_.join)
      _ <- IO(println(result))
    } yield ExitCode.Success
  }
}
