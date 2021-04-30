package db

import cats.effect._
import cats.implicits._
import db.Homework._
import doobie.Transactor
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

import scala.concurrent.ExecutionContext.global

object HttpServer {
  def start(xa: Transactor[IO])(implicit ce: ConcurrentEffect[IO], t: Timer[IO]): IO[ExitCode] = {
    val books = BooksStorage(xa)
    val authors = AuthorsStorage(xa)
    val app = (booksRoutes(books) <+> authorsRoutes(authors)).orNotFound
    BlazeServerBuilder[IO](global)
      .withHttpApp(app)
      .bindHttp(port = 9008, host = "0.0.0.0")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  def booksRoutes(storage: UpdatableStorage[IO, Book]): HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "book" / UUIDVar(id) => storage.read(id).flatMap(_.fold(NotFound())(Ok(_)))

    case GET -> Root / "books" / "list" => storage.readAll().flatMap(Ok(_))

    case req@POST -> Root / "book" => for {
      book <- req.as[Book]
      res <- storage.create(book)
      resp <- Ok(res)
    } yield resp

    case req@PUT -> Root / "book" => for {
      book <- req.as[Book]
      res <- storage.update(book)
      resp <- Ok(res)
    } yield resp

    case DELETE -> Root / "book" / UUIDVar(id) => storage.delete(id).flatMap(Ok(_))
  }

  def authorsRoutes(storage: Storage[IO, Author]): HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "author" / UUIDVar(id) => storage.read(id).flatMap(_.fold(NotFound())(Ok(_)))

    case GET -> Root / "authors" / "list" => storage.readAll().flatMap(Ok(_))

    case req@POST -> Root / "author" => for {
      author <- req.as[Author]
      res <- storage.create(author)
      resp <- Ok(res)
    } yield resp
  }
}
