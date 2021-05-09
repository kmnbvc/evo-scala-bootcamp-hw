package effects

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.concurrent.ExecutionContext
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
    def of[A, B](fs: List[Worker[A, B]])(implicit blocker: Blocker): IO[WorkerPool[A, B]] = {
      val freeWorkers = new ConcurrentLinkedQueue[Worker[A, B]]()
      fs.foreach(w => freeWorkers.add(w))

      def worker(): Resource[IO, Worker[A, B]] = {
        val acquire = Concurrent[IO].untilDefinedM(IO(Option(freeWorkers.poll())))
        val release = (w: Worker[A, B]) => IO(freeWorkers.add(w))
        Resource.make(acquire <* IO(println("worker obtained")))(release(_) *> IO(println("returning worker")))
      }

      IO { a =>
        blocker.blockOn(worker().use(_.apply(a)))
      }
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    val blockerEc = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    implicit val blocker: Blocker = Blocker.liftExecutorService(blockerEc)

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
      _ <- IO(blockerEc.shutdown())
    } yield ExitCode.Success
  }
}
