package http

import cats.data.OptionT
import cats.effect._
import cats.effect.concurrent.Ref
import fs2.Stream
import fs2.concurrent.Queue
import http.MessageWS._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.client.jdkhttpclient._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.net.http.HttpClient
import java.security.SecureRandom

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
object GuessServerWS extends IOApp {

  private val random = SecureRandom.getInstanceStrong

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](executionContext)
      .withHttpApp(makeApp().orNotFound)
      .bindHttp(port = 9009, host = "0.0.0.0")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  def makeApp(): HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root / "start" / "ws" => for {
        queue <- Queue.unbounded[IO, WebSocketFrame]
        (correct, bounds) = generateNumber()
        starting = Stream.emit(WebSocketFrame.Text(bounds.asJson.noSpaces))
        send = starting.merge(queue.dequeue.map(respond(_, correct)))
        receive = queue.enqueue
        resp <- WebSocketBuilder[IO].build(send, receive)
      } yield resp
    }
  }

  def respond(frame: WebSocketFrame, correct: Int): WebSocketFrame = frame match {
    case WebSocketFrame.Text(text, _) =>
      decode[Guess](text) match {
        case Right(Guess(x)) => WebSocketFrame.Text(answer(correct, x).asJson.noSpaces)
        case Left(err) => WebSocketFrame.Text(err.getMessage)
      }
    case _ => WebSocketFrame.Text("not supported")
  }

  def answer(correct: Int, guess: Int): GuessAnswer = {
    if (guess == correct) Correct
    else if (guess > correct) Greater
    else Lower
  }

  def generateNumber(): (Int, StartingBounds) = {
    val x = random.nextInt(999) + 500
    val min = x - random.nextInt(500)
    val max = x + random.nextInt(500)
    (x, StartingBounds(min, max))
  }
}

object GuessClientWS extends IOApp {
  private val uri = uri"ws://localhost:9009/start/ws"

  final case class GameState(min: Int, max: Int, attempt: Int = 0) {
    def guess: Guess = Guess((min + max) / 2)
    def lower: GameState = GameState(guess.x, max, attempt + 1)
    def greater: GameState = GameState(min, guess.x, attempt + 1)
  }
  final case class Result(x: Int, state: GameState)

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use[IO, ExitCode] { client =>
      OptionT(client.receive).semiflatMap(parseJson[StartingBounds](_))
        .flatMap(guess(_, client))
        .foldF(client.sendClose("error"))(x => IO(println(x)))
        .as(ExitCode.Success)
    }
  }

  private def parseJson[T: Decoder](frame: WSDataFrame): IO[T] = frame match {
    case WSFrame.Text(text, _) => IO.fromEither(decode[T](text))
    case _ => IO.raiseError(new RuntimeException(s"Unexpected frame type: $frame"))
  }

  def guess(bounds: StartingBounds, client: WSConnectionHighLevel[IO]): OptionT[IO, Result] = {
    val res = for {
      initial <- IO.pure(GameState(bounds.min, bounds.max))
      storage <- Ref.of[IO, GameState](initial)
      _ <- client.send(WSFrame.Text(initial.guess.asJson.noSpaces))
      result <- untilCorrect(storage, client).compile.last
    } yield result

    OptionT(res)
  }

  def untilCorrect(storage: Ref[IO, GameState], client: WSConnectionHighLevel[IO]): Stream[IO, Result] = {
    client.receiveStream.evalMap(parseJson[GuessAnswer](_)).evalMap {
      case Correct => storage.get.map(state => Result(state.guess.x, state))
      case Lower => for {
        state <- storage.updateAndGet(_.lower)
        _ <- client.send(WSFrame.Text(state.guess.asJson.noSpaces))
      } yield ()
      case Greater => for {
        state <- storage.updateAndGet(_.greater)
        _ <- client.send(WSFrame.Text(state.guess.asJson.noSpaces))
      } yield ()
    }.collectFirst {
      case r@Result(_, _) => r
    }
  }
}

object MessageWS {
  sealed trait GuessAnswer
  case object Greater extends GuessAnswer
  case object Lower extends GuessAnswer
  case object Correct extends GuessAnswer

  sealed trait GameStep
  final case class StartingBounds(min: Int, max: Int) extends GameStep
  final case class Guess(x: Int) extends GameStep

  object GuessAnswer {
    implicit val jsonCodec: Codec[GuessAnswer] = deriveCodec
  }

  object GameStep {
    implicit val jsonCodec: Codec[GameStep] = deriveCodec
    implicit val startingBoundsJsonCodec: Codec[StartingBounds] = deriveCodec
    implicit val guessJsonCodec: Codec[Guess] = deriveCodec
  }
}
