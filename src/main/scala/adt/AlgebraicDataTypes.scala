package adt

import scala.collection.SortedSet

object AlgebraicDataTypes {

  sealed trait Suit
  object Suit {
    case object Diamonds extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit
    case object Clubs extends Suit
  }

  sealed trait Rank
  object Rank {
    case object Two extends Rank
    case object Three extends Rank
    case object Four extends Rank
    case object Five extends Rank
    case object Six extends Rank
    case object Seven extends Rank
    case object Eight extends Rank
    case object Nine extends Rank
    case object Ten extends Rank
    case object Jack extends Rank
    case object Queen extends Rank
    case object King extends Rank
    case object Ace extends Rank
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed trait Category
  object Category {
    case object HighCard extends Category
    case object Pair extends Category
    case object TwoPairs extends Category
    case object ThreeOfAKind extends Category
    case object Straight extends Category
    case object Flush extends Category
    case object FullHouse extends Category
    case object FourOfAKind extends Category
    case object StraightFlush extends Category
  }

  sealed trait Board
  sealed trait Hand

  final case class FiveCardBoard private(cards: List[Card]) extends Board
  object FiveCardBoard {
    def apply(cards: List[Card]): Either[CombinationError, FiveCardBoard] = {
      Either.cond(cards.length == 5, new FiveCardBoard(cards), IncorrectCardsNumber("must be 5 cards"))
    }
  }

  final case class TexasHoldemHand private(cards: List[Card]) extends Hand
  object TexasHoldemHand {
    def apply(cards: List[Card]): Either[CombinationError, TexasHoldemHand] = {
      Either.cond(cards.length == 2, new TexasHoldemHand(cards), IncorrectCardsNumber("must be 2 cards"))
    }
  }

  final case class OmahaHoldemHand private(cards: List[Card]) extends Hand
  object OmahaHoldemHand {
    def apply(cards: List[Card]): Either[CombinationError, OmahaHoldemHand] = {
      Either.cond(cards.length == 4, new OmahaHoldemHand(cards), IncorrectCardsNumber("must be 4 cards"))
    }
  }

  sealed trait CombinationError
  final case class IncorrectCardsNumber(msg: String) extends CombinationError

  final case class TestCase(board: Board, hands: List[Hand])
  final case class TestResult(board: Board, hands: SortedSet[Hand])
}
