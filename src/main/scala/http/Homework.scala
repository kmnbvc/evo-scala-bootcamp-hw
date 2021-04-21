package http

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import http.Message._
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.middleware.Logger

import java.security.SecureRandom
import java.util.UUID
import scala.concurrent.ExecutionContext.global

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.
object GuessServer extends IOApp {

  object valueParam extends QueryParamDecoderMatcher[Int]("value")
  object gameIdParam extends QueryParamDecoderMatcher[String]("gameId")

  private val random = SecureRandom.getInstanceStrong

  def run(args: List[String]): IO[ExitCode] = for {
    app <- makeApp()
    srv <- BlazeServerBuilder[IO](global)
      .withHttpApp(app)
      .bindHttp(port = 9009, host = "localhost")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  } yield srv

  def makeApp(): IO[HttpApp[IO]] = for {
    storage <- Ref.of[IO, Map[String, Int]](Map.empty)
    rts = routes(storage).orNotFound
    app = Logger.httpApp(logHeaders = true, logBody = true)(rts)
  } yield app

  def routes(storage: Ref[IO, Map[String, Int]]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case GET -> Root / "start" =>
        val (id, number, bounds) = generateNumber()
        storage.update(_ + (id -> number)) >> Ok((id, bounds))

      case GET -> Root / "guess" :? valueParam(value) :? gameIdParam(id) => for {
        result <- storage.modify { values =>
          val updated = values.updatedWith(id)(x => x.filterNot(_ == value))
          val a = values.get(id).map(answer(_, value))
          (updated, a)
        }
        resp <- result.map(Ok(_)).getOrElse(NotFound(s"No such game id: $id"))
      } yield resp
    }
  }

  def generateNumber(): (String, Int, StartingBounds) = {
    val x = random.nextInt(999) + 500
    val min = x - random.nextInt(500)
    val max = x + random.nextInt(500)
    (UUID.randomUUID().toString, x, StartingBounds(min, max))
  }

  def answer(correct: Int, guess: Int): GuessAnswer = {
    if (guess == correct) Correct
    else if (guess > correct) Greater
    else Lower
  }
}

object GuessClient extends IOApp {

  private val baseUri = uri"http://localhost:9009"
  private val startUri = baseUri / "start"
  private def guessUri(id: String, value: Int): Uri = {
    (baseUri / "guess").withQueryParams(Map("value" -> value.toString, "gameId" -> id))
  }

  def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[(String, StartingBounds)](startUri).flatMap {
        case (id, StartingBounds(min, max)) => guess(id, min, max)(client)
      }
    }.map(println).as(ExitCode.Success)
  }

  def guess(id: String, min: Int, max: Int, attempt: Int = 0)(client: Client[IO]): IO[Result] = {
    val x = (min + max) / 2
    println(s"game: $id; current bounds: ($min, $max); x: $x; attempt: $attempt")
    client.expect[GuessAnswer](guessUri(id, x)).flatMap {
      case Correct => Result(x, attempt).pure[IO]
      case Greater => guess(id, min, x, attempt + 1)(client)
      case Lower => guess(id, x, max, attempt + 1)(client)
    }
  }
}

object Message {
  sealed trait GuessAnswer
  case object Greater extends GuessAnswer
  case object Lower extends GuessAnswer
  case object Correct extends GuessAnswer

  sealed trait GameStep
  final case class StartingBounds(min: Int, max: Int) extends GameStep
  final case class Guess(x: Int) extends GameStep
  final case class Result(guessed: Int, attempts: Int) extends GameStep

  object GuessAnswer {
    implicit val jsonCodec: Codec[GuessAnswer] = deriveCodec
  }

  object GameStep {
    implicit val jsonCodec: Codec[GameStep] = deriveCodec
    implicit val startingBoundsJsonCodec: Codec[StartingBounds] = deriveCodec
  }
}
