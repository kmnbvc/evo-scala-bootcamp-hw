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

  sealed trait CombinationType
  sealed trait Board extends CombinationType
  sealed trait Hand extends CombinationType
  object CombinationType {
    case object TexasHoldemHand extends Hand
    case object TexasHoldemBoard extends Board
    case object OmahaHoldemHand extends Hand
    case object OmahaHoldemBoard extends Board
    case object MatchedCombination extends CombinationType
  }

  final case class Combination[T <: CombinationType] private(cards: List[Card], ctype: T)
  object Combination {
    import CombinationType._

    def apply[T <: CombinationType](cards: List[Card], ctype: T): Either[CombinationError, Combination[T]] = {
      val checkingSizeEq = (size: Int) => Either.cond(cards.length == size, new Combination(cards, ctype), IncorrectCombinationSize(s"must be $size cards"))

      ctype match {
        case OmahaHoldemBoard => checkingSizeEq(5)
        case OmahaHoldemHand => checkingSizeEq(4)
        case TexasHoldemBoard => checkingSizeEq(5)
        case TexasHoldemHand => checkingSizeEq(2)
        case MatchedCombination => Either.cond(cards.length >= 2, new Combination(cards, ctype), IncorrectCombinationSize("must be at least 2 cards"))
        case _ => Left(UnsupportedCombinationType(ctype))
      }
    }
  }

  sealed trait CombinationError
  final case class IncorrectCombinationSize(msg: String) extends CombinationError
  final case class UnsupportedCombinationType[T <: CombinationType](ctype: T) extends CombinationError

  final case class TestCase(board: Combination[_ <: Board], hands: List[Combination[_ <: Hand]])
  final case class TestResult(board: Combination[_ <: Board], hands: SortedSet[Combination[_ <: Hand]])
}
