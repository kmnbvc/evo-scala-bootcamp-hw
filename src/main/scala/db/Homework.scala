package db

import cats.effect._
import doobie._
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.postgres.implicits.pgEnumStringOpt

import java.time.{LocalDate, Year}
import java.util.UUID

/*
  Нужно написать небольшой CRUD для сущностей из лекции. Добавить в Book поле genre.
  Можно использовать любую библиотеку для работы с http (http4s, akka-http и т.д.).
  Create/Read_all/Read_one нужны для всех сущностей. Update можно ограничиться только Book.
  Бонусные очки:
  Пару фильтров для Book и/или Author (год, имя/название и т.д.)
  Возвращать BookWithInfo вместо Book
  Delete
 */
object Homework {
  sealed trait Storage[F[_], T] {
    def create(entity: T): F[Int]
    def read(id: UUID): F[Option[T]]
    def readAll(): F[List[T]]
  }

  sealed trait UpdatableStorage[F[_], T] extends Storage[F, T] {
    def update(entity: T): F[Int]
    def delete(id: UUID): F[Int]
  }

  object BooksStorage {
    def apply(xa: Transactor[IO]): UpdatableStorage[IO, Book] = new UpdatableStorage[IO, Book] {
      override def update(entity: Book): IO[Int] = {
        val (id, authorId, title, year, genre) = (entity.id,
          entity.authorId, entity.title, entity.year, entity.genre)

        sql"update books set authorId = $authorId, title = $title, year = $year, genre = $genre where id = $id"
          .update.run.transact(xa)
      }
      override def create(entity: Book): IO[Int] = {
        val (id, authorId, title, year, genre) = (UUID.randomUUID(),
          entity.authorId, entity.title, entity.year, entity.genre)

        sql"insert into books (id, authorId, title, year, genre) values ($id, $authorId, $title, $year, $genre)"
          .update.run.transact(xa)
      }
      override def read(id: UUID): IO[Option[Book]] = {
        sql"select * from books where id = $id".query[Book].option.transact(xa)
      }
      override def readAll(): IO[List[Book]] = {
        sql"select * from books".query[Book].to[List].transact(xa)
      }
      override def delete(id: UUID): IO[Int] = {
        sql"delete from books where id = $id".update.run.transact(xa)
      }
    }
  }

  object AuthorsStorage {
    def apply(xa: Transactor[IO]): Storage[IO, Author] = new Storage[IO, Author] {
      override def create(entity: Author): IO[Int] = {
        val (id, name, birthday) = (UUID.randomUUID(), entity.name, entity.birthday)
        sql"insert into authors (id, name, birthday) values ($id, $name, $birthday)".update.run.transact(xa)
      }
      override def read(id: UUID): IO[Option[Author]] = {
        sql"select * from authors where id = $id".query[Author].option.transact(xa)
      }
      override def readAll(): IO[List[Author]] = {
        sql"select * from authors".query[Author].to[List].transact(xa)
      }
    }
  }

  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  implicit val genreMeta: Meta[Genre] = pgEnumStringOpt("book_genre", Genre.fromEnum, Genre.toEnum)
  implicit val localDateMeta: Meta[LocalDate] = localdate.JavaTimeLocalDateMeta

}
