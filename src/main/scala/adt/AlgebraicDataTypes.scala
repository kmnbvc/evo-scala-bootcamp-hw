package adt

import scala.collection.SortedSet

object AlgebraicDataTypes {

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type

  case class Suit private (name: String)
  object Suit {
    private val suits = Set("s", "d", "r", "c")
    def apply(s: String): Option[Suit] = {
      Option.when(suits.contains(s))(new Suit(s))
    }
  }

  case class Rank private (value: String)
  object Rank {
    private val ranks = (1 to 10).map(_.toString).toSet ++ Set("J", "Q", "K", "A")
    def apply(s: String): Option[Rank] = {
      Option.when(ranks.contains(s))(new Rank(s))
    }
  }

  case class Card(suit: Suit, rank: Rank)

  case class Combination private (cards: List[Card])
  object Combination {
    def apply(cards: List[Card]): Either[String, Combination] = {
      Either.cond(cards.nonEmpty, new Combination(cards), "must be more than 0 cards")
    }
  }

  abstract sealed case class SizedCombination[T] protected (cards: List[Card])
  object SizedCombination {
    def apply[T <: SizedCombination[T]](cards: List[Card])(implicit ev: Manifest[T]): Either[String, T] = {
      ev.runtimeClass match {
        case TexasHoldemHand.getClass => Either.cond(cards.length == 2, new TexasHoldemHand(cards), "must be 2 cards")
        case TexasHoldemBoard.getClass => Either.cond(cards.length == 5, new TexasHoldemBoard(cards), "must be 5 cards")
      }
    }
  }

  case class TexasHoldemHand(override val cards: List[Card]) extends SizedCombination[TexasHoldemHand](cards)
  case class TexasHoldemBoard(override val cards: List[Card]) extends SizedCombination[TexasHoldemBoard](cards)

  val z = SizedCombination[TexasHoldemHand](List[Card]())

//  case class TestCase(board: Board, hands: List[Hand])
//  case class TestResult(board: Board, hands: SortedSet[Card])

}
