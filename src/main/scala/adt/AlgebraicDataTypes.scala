package adt

import scala.collection.SortedSet

object AlgebraicDataTypes {

  final case class Suit private(name: String)
  object Suit {
    private val suits = Set("s", "d", "h", "c")
    def apply(s: String): Option[Suit] = Option.when(suits.contains(s))(new Suit(s))
  }

  final case class Rank private(value: Int)
  object Rank {
    private val ranks = Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 11, "T" -> 10) ++ (2 to 9).map(n => n.toString -> n)
    def apply(s: String): Option[Rank] = Option.when(ranks.contains(s))(new Rank(ranks(s)))
  }

  final case class Card(suit: Suit, rank: Rank)

  trait CombinationType
  trait Board extends CombinationType
  trait Hand extends CombinationType
  object CombinationType {
    final case class TexasHoldemHand() extends Hand
    final case class TexasHoldemBoard() extends Board
    final case class OmahaHoldemHand() extends Hand
    final case class OmahaHoldemBoard() extends Board
    final case class MatchedCombination() extends CombinationType
  }

  final case class Combination[+T <: CombinationType] private(cards: List[Card], ctype: T)
  object Combination {
    def apply[T](cards: List[Card], ctype: T): Either[CombinationError, Combination[T]] = {
      val checkingSizeEq = (size: Int) => Either.cond(cards.length == size, new Combination(cards, ctype), IncorrectCombinationSize(s"must be $size cards"))

      ctype match {
        case CombinationType.OmahaHoldemBoard() => checkingSizeEq(5)
        case CombinationType.OmahaHoldemHand() => checkingSizeEq(4)
        case CombinationType.TexasHoldemBoard() => checkingSizeEq(5)
        case CombinationType.TexasHoldemHand() => checkingSizeEq(2)
        case CombinationType.MatchedCombination() => Either.cond(cards.length >= 2, new Combination(cards, ctype), IncorrectCombinationSize("must be at least 2 cards"))
        case _ => Left(UnsupportedCombinationType(ctype))
      }
    }
  }

  trait CombinationError
  final case class IncorrectCombinationSize(msg: String) extends CombinationError
  final case class UnsupportedCombinationType[T <: CombinationType](ctype: T) extends CombinationError

  final case class TestCase(board: Combination[Board], hands: List[Combination[Hand]])
  final case class TestResult(board: Combination[Board], hands: SortedSet[Combination[Hand]])
}
