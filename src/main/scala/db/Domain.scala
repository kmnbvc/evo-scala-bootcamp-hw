package db

import io.circe.Codec
import io.circe.generic.extras.semiauto.deriveEnumerationCodec
import io.circe.generic.semiauto.deriveCodec

import java.time.{LocalDate, Year}
import java.util.UUID

final case class Author(id: UUID, name: String, birthday: LocalDate)

final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: Genre)

final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year, genre: Genre) {
  override def toString: String = s"$title ($year) by ${author.name}"
}

sealed trait Genre
case object Drama extends Genre
case object Folk extends Genre
case object Poetry extends Genre
case object Unknown extends Genre

object Book {
  implicit val jsonCodec: Codec[Book] = deriveCodec
}

object Author {
  implicit val jsonCodec: Codec[Author] = deriveCodec
}

object Genre {
  implicit val jsonCodec: Codec[Genre] = deriveEnumerationCodec

  def fromEnum(x: Int): Genre = x match {
    case 0 => Drama
    case 1 => Folk
    case 2 => Poetry
    case 3 => Unknown
  }

  def toEnum(g: Genre): Int = g match {
    case Drama => 0
    case Folk => 1
    case Poetry => 2
    case Unknown => 3
  }
}

object BookWithAuthor {
  implicit val jsonCodec: Codec[BookWithAuthor] = deriveCodec

  def apply(author: Author, book: Book): BookWithAuthor = book match {
    case Book(id, _, title, year, genre) => BookWithAuthor(id, author, title, year, genre)
  }
}
