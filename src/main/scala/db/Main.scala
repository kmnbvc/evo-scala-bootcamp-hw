package db

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import doobie._
import doobie.implicits._

object Main extends IOApp {

  import DbCommon._
  import Homework._

  def run(args: List[String]): IO[ExitCode] = {
    DbTransactor.pooled[IO].use { xa =>

      val ddl1 = Fragment.const(createGenreEnum)
      val ddl2 = Fragment.const(createTableAuthorsSql)
      val ddl3 = Fragment.const(createTableBooksSql)
      val dml = Fragment.const(populateDataSql)

      def setup(): ConnectionIO[Unit] =
        for {
          _ <- ddl1.update.run
          _ <- ddl2.update.run
          _ <- ddl3.update.run
          _ <- dml.update.run
        } yield ()

      for {
        _ <- setup().transact(xa)
        _ <- Fragment.const(fetchAuthorsSql).query[Author].to[List].transact(xa) >>= printLine
        _ <- Fragment.const(fetchBooksSql).query[Book].to[List].transact(xa) >>= printLine
        res <- HttpServer.start(xa)
      } yield res
    }
  }

  def printLine(x: Any): IO[Unit] = IO(println(x))
}
