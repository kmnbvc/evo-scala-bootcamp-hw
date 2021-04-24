package http

import cats.data.OptionT
import cats.effect._
import cats.effect.concurrent.Ref
import fs2.Stream
import fs2.concurrent.Queue
import http.MessageWS._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
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
        starting = WebSocketFrame.Text(bounds.asJson.noSpaces)
        send = Stream.emit(starting).merge(queue.dequeue.map(handleClientMessage(_, correct)))
        receive = queue.enqueue
        resp <- WebSocketBuilder[IO].build(send, receive)
      } yield resp
    }
  }

  def handleClientMessage(frame: WebSocketFrame, correct: Int): WebSocketFrame = frame match {
    case WebSocketFrame.Text(text, _) =>
      decode[Guess](text).map(g => answer(correct, g.x)).fold(
        e => WebSocketFrame.Text(e.getMessage),
        a => WebSocketFrame.Text(a.asJson.noSpaces)
      )
    case _ => frame
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

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use[IO, ExitCode] { client =>
      OptionT(client.receive).subflatMap(parseBounds)
        .flatMap(bounds => guess(bounds.min, bounds.max)(client))
        .foldF(client.sendClose("error"))(x => IO(println(x)))
        .as(ExitCode.Success)
    }
  }

  private def parseBounds(frame: WSDataFrame): Option[StartingBounds] = frame match {
    case WSFrame.Text(text, _) => decode[StartingBounds](text).toOption
    case _ => None
  }

  private def parseAnswer(frame: WSDataFrame): Option[GuessAnswer] = frame match {
    case WSFrame.Text(text, _) => decode[GuessAnswer](text).toOption
    case _ => None
  }

  def guess(min: Int, max: Int)(client: WSConnectionHighLevel[IO]): OptionT[IO, Result] = {
    val x = (min + max) / 2
    println(s"current bounds: ($min, $max); x: $x")

    def m(storage: Ref[IO, Guess]): Stream[IO, Result] = {
      val z: Stream[IO, GameStep] = client.receiveStream.map(parseAnswer).flatMap {
        case Some(Correct) => Stream.eval(storage.get.map(g => Result(g.x)))
        case Some(Lower) =>
          val guess = storage.updateAndGet(gg => Guess((gg.x + gg.max) / 2, gg.x, gg.max))
          val sent = guess.flatMap(gg => client.send(WSFrame.Text(gg.asJson.noSpaces)).as(gg))
          Stream.eval(sent)
        case Some(Greater) =>
          val guess = storage.updateAndGet(gg => Guess((gg.min + gg.x) / 2, gg.min, gg.x))
          val sent = guess.flatMap(gg => client.send(WSFrame.Text(gg.asJson.noSpaces)).as(gg))
          Stream.eval(sent)
        case None => Stream.empty
      }

      z.collectFirst {
        case r@Result(_) => r
      }
    }

    val res = for {
      _ <- client.send(WSFrame.Text(Guess(x, min, max).asJson.noSpaces))
      storage <- Ref.of[IO, Guess](Guess(x, min, max))
      res = m(storage)
      r = res.compile.last
    } yield r

    OptionT(res.flatMap(x => x))
  }
}

object MessageWS {
  sealed trait GuessAnswer
  case object Greater extends GuessAnswer
  case object Lower extends GuessAnswer
  case object Correct extends GuessAnswer

  sealed trait GameStep
  final case class StartingBounds(min: Int, max: Int) extends GameStep
  final case class Guess(x: Int, min: Int, max: Int) extends GameStep
  final case class Result(x: Int) extends GameStep

  final case class GameState()

  object GuessAnswer {
    implicit val jsonCodec: Codec[GuessAnswer] = deriveCodec
  }

  object GameStep {
    implicit val jsonCodec: Codec[GameStep] = deriveCodec
    implicit val startingBoundsJsonCodec: Codec[StartingBounds] = deriveCodec
    implicit val guessJsonCodec: Codec[Guess] = deriveCodec
  }
}
