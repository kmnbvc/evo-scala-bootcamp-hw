package db

import io.circe.Codec
import io.circe.generic.extras.semiauto.deriveEnumerationCodec
import io.circe.generic.semiauto.deriveCodec

import java.time.{LocalDate, Year}
import java.util.UUID

final case class Author(id: UUID, name: String, birthday: LocalDate)

final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: Genre)

final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
  override def toString: String = s"$title ($year) by ${author.name}"
}

sealed trait Genre
case object Drama extends Genre
case object Folk extends Genre
case object Poetry extends Genre

object Book {
  implicit val jsonCodec: Codec[Book] = deriveCodec
}

object Author {
  implicit val jsonCodec: Codec[Author] = deriveCodec
}

object Genre {
  implicit val jsonCodec: Codec[Genre] = deriveEnumerationCodec

  def fromEnum(str: String): Option[Genre] = str match {
    case "drama" => Some(Drama)
    case "folk" => Some(Folk)
    case "poetry" => Some(Poetry)
    case _ => None
  }

  def toEnum(g: Genre): String = g match {
    case Drama => "drama"
    case Folk => "folk"
    case Poetry => "poetry"
  }
}
