package db

import cats.effect._
import doobie._
import doobie.implicits._
import doobie.implicits.legacy.localdate

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
  sealed trait Read[F[_], T] {
    def read(id: UUID): F[Option[T]]
    def readAll(): F[List[T]]
  }
  sealed trait Create[F[_], T] {
    def create(entity: T): F[Int]
  }
  sealed trait Update[F[_], T] {
    def update(entity: T): F[Int]
  }
  sealed trait Delete[F[_], T] {
    def delete(id: UUID): F[Int]
  }

  sealed trait AuthorsStorage[F[_]] extends Create[F, Author]
    with Delete[F, Author] with Read[F, Author]
  sealed trait BooksStorage[F[_]] extends Create[F, Book]
    with Update[F, Book] with Delete[F, Book] with Read[F, BookWithAuthor]

  object BooksStorage {
    private val selectFr = fr"select a.*, b.* from books b inner join authors a on b.authorId = a.id"
    private val whereId = (id: UUID) => fr"where b.id = $id"

    def apply(xa: Transactor[IO]): BooksStorage[IO] = new BooksStorage[IO] {
      def update(entity: Book): IO[Int] = update {
        fr"update books set" ++
          fr"authorId = ${entity.authorId}, " ++
          fr"title = ${entity.title}," ++
          fr"year = ${entity.year}," ++
          fr"genre = ${entity.genre}" ++
          fr"where id = ${entity.id}"
      }

      def create(entity: Book): IO[Int] = update {
        fr"insert into books (id, authorId, title, year, genre) " ++
          fr"values (" ++
          fr"${entity.id}," ++
          fr"${entity.authorId}," ++
          fr"${entity.title}," ++
          fr"${entity.year}," ++
          fr"${entity.genre})"
      }

      def read(id: UUID): IO[Option[BookWithAuthor]] = query(selectFr ++ whereId(id)).map(_.headOption)
      def readAll(): IO[List[BookWithAuthor]] = query(selectFr)
      def delete(id: UUID): IO[Int] = update(sql"delete from books where id = $id")

      private def query(sql: Fragment): IO[List[BookWithAuthor]] =
        sql.query[(Author, Book)].map {
          case (author, book) => BookWithAuthor(author, book)
        }.to[List].transact(xa)

      private def update(sql: Fragment): IO[Int] = sql.update.run.transact(xa)
    }
  }

  object AuthorsStorage {
    private val selectFr = fr"select * from authors"
    private val whereId = (id: UUID) => fr"where id = $id"

    def apply(xa: Transactor[IO]): AuthorsStorage[IO] = new AuthorsStorage[IO] {
      def create(entity: Author): IO[Int] = {
        val sql = fr"insert into authors (id, name, birthday) " ++
          fr"values (" ++
          fr"${entity.id}," ++
          fr"${entity.name}," ++
          fr"${entity.birthday})"
        update(sql)
      }
      def read(id: UUID): IO[Option[Author]] = query(selectFr ++ whereId(id)).map(_.headOption)
      def readAll(): IO[List[Author]] = query(selectFr)
      def delete(id: UUID): IO[Int] = update(sql"delete from authors where id = $id")

      private def query(sql: Fragment): IO[List[Author]] = sql.query[Author].to[List].transact(xa)
      private def update(sql: Fragment): IO[Int] = sql.update.run.transact(xa)
    }
  }

  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  implicit val localDateMeta: Meta[LocalDate] = localdate.JavaTimeLocalDateMeta

  implicit val genreMeta: Meta[Genre] = Meta[Int].timap(Genre.fromEnum)(Genre.toEnum)
}
