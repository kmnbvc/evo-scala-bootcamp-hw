package effects

import cats.effect._
import cats.implicits._

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.StdIn

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.
  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
 */
object EffectsHomework2 extends IOApp {

  private val cores = Runtime.getRuntime.availableProcessors()
  private val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(cores))
  private val cs = IO.contextShift(ec)

  def run(args: List[String]): IO[ExitCode] = {
    import Hashing._
    import Reader._

    val input = for {
      _ <- IO.shift(ec)
      path <- filePathReader.retrying
      seed <- seedReader.retrying
      content <- IO(Files.readString(path))
      words <- IO(content.split("\\s+").toList)
    } yield (words, seed)

    val hashing = List(javaHash _, knuthHash _)

    val result = input.flatMap {
      case (words, seed) => hashing.traverse(f => signature(words, seed, f)(cs))
    }

    result.map(xs => println(xs.flatten.minBy(_.hash))).guarantee(IO(ec.shutdown())).as(ExitCode.Success)
  }

  object Hashing {
    final case class Result(word: String, hash: Int)

    def signature(words: List[String], seed: Int, f: (String, Int) => Int)(implicit cs: ContextShift[IO]): IO[List[Result]] = {
      words.traverse(w => IO(hash(w, seed, f)).start(cs))
        .flatMap(_.traverse(_.join))
    }

    def hash(word: String, seed: Int, f: (String, Int) => Int): Result = Result(word, f(word, seed))

    def javaHash(word: String, seed: Int = 0): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = 31 * hash + ch.toInt
      hash = hash ^ (hash >> 20) ^ (hash >> 12)
      hash ^ (hash >> 7) ^ (hash >> 4)
    }

    def knuthHash(word: String, constant: Int): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
      hash % constant
    }
  }

  trait Reader[F[_], T] {
    def read(): F[T]
  }

  object Reader {
    implicit class ReaderOps[F[_] : Sync, T](r: Reader[F, T]) {
      def retrying(implicit logger: Logger[F]): F[T] = {
        def iter: F[T] = r.read().handleErrorWith(t => logger.write(t.getMessage) >> iter)
        iter
      }
    }

    val filePathReader: Reader[IO, Path] = () => IO(StdIn.readLine("File path:")).map(Path.of(_)).flatMap { path =>
      IO.raiseWhen(Files.notExists(path))(new FileNotFoundException(s"file not found: $path")) >>
        IO.raiseUnless(Files.isReadable(path))(new IllegalArgumentException(s"cannot read file: $path")) >>
        IO.pure(path)
    }

    val seedReader: Reader[IO, Int] = () => IO(StdIn.readLine("Seed:")).map(_.toInt)
  }

  trait Logger[F[_]] {
    def write(x: Any): F[Unit]
  }

  object Logger {
    implicit val stdOutLogger: Logger[IO] = x => IO(println(x))
  }
}
