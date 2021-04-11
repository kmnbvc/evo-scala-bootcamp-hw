package testing

import atto.Parser
import atto.parser.character._
import atto.parser.combinator.endOfInput
import atto.parser.numeric.double
import atto.parser.text._
import atto.syntax.parser._

object Json {
  sealed trait Json
  case object JNull extends Json
  final case class JBoolean(value: Boolean) extends Json
  final case class JNumber(value: Double) extends Json
  final case class JString(value: String) extends Json
  final case class JArray(value: Vector[Json]) extends Json
  final case class JObject(value: Map[String, Json]) extends Json

  object Parser {
    val jNull: Parser[JNull.type] =
      string("null") >| JNull
    val jBoolean: Parser[JBoolean] =
      (string("true") >| true | string("false") >| false) -| JBoolean
    val jNumber: Parser[JNumber] =
      double -| JNumber
    val jString: Parser[JString] =
      stringLiteral -| JString
    lazy val jArray: Parser[JArray] =
      (char('[') ~> json.sepBy(char(',')) <~ char(']')) -| { l =>
        JArray(l.toVector)
      }
    lazy val jObject: Parser[JObject] =
      braces {
        val name = stringLiteral
        val sep = skipWhitespace ~> char(':') <~ skipWhitespace
        ((name <~ sep) ~ json).sepBy(char(','))
      } -| {
        l => JObject(l.toMap)
      }
    lazy val json: Parser[Json] =
      jNull | jBoolean | jNumber | jString | jArray | jObject
    lazy val jsonOnly: Parser[Json] =
      json <~ endOfInput
  }

  def parse(s: String): Option[Json] =
    Parser.jsonOnly.parseOnly(s).option

  def print(json: Json): String = json match {
    case JNull => "null"
    case JBoolean(value) => value.toString
    case JNumber(value) => value.toString
    case JString(value) => quote(value)
    case JArray(values) => values.map(print).mkString("[", ",", "]")
    case JObject(values) => values.map {
      case (field, value) => s"${quote(field)}:${print(value)}"
    }.mkString("{", ",", "}")
  }

  def quote(s: String): String = "\"" + s + "\""
}
