package db

import java.util.UUID

object DbCommon {

  val authorOdersky: UUID = UUID.randomUUID()
  val authorRowling: UUID = UUID.randomUUID()
  val bookScala: UUID = UUID.randomUUID()
  val bookHPStone: UUID = UUID.randomUUID()
  val bookHPSecrets: UUID = UUID.randomUUID()

  val createGenreEnum: String =
    """create type book_genre as enum ('drama', 'folk', 'poetry')
      |""".stripMargin

  val createTableAuthorsSql: String =
    """CREATE TABLE authors (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL,
      |  birthday DATE);""".stripMargin

  val createTableBooksSql: String =
    """CREATE TABLE books (
      |  id UUID PRIMARY KEY,
      |  author UUID NOT NULL,
      |  title VARCHAR(100) NOT NULL,
      |  year INT,
      |  genre book_genre NOT NULL,
      |  FOREIGN KEY (author) REFERENCES authors(id));""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO authors (id, name, birthday) VALUES
       |  ('$authorOdersky', 'Martin Odersky', '1958-09-05'),
       |  ('$authorRowling', 'J.K. Rowling', '1965-07-31');
       |
       |INSERT INTO books (id, author, title, year, genre) VALUES
       |  ('$bookScala', '$authorOdersky', 'Programming in Scala', 2016, 'drama'),
       |  ('$bookHPStone', '$authorRowling', 'Harry Potter and Philosopher''s Stone', 1997, 'folk'),
       |  ('$bookHPSecrets', '$authorRowling', 'Harry Potter and the Chamber of Secrets', 1998, 'poetry');
       |""".stripMargin

  val fetchBooksSql: String =
    """SELECT b.id, a.id, b.title, b.year, b.genre FROM books b
      |INNER JOIN authors a ON b.author = a.id """.stripMargin

  val fetchAuthorsSql: String =
    """SELECT a.id, a.name, a.birthday FROM authors a
      |""".stripMargin
}
