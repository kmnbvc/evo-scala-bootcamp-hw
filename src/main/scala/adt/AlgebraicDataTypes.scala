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
  object CombinationType {
    final case class TexasHoldemHand() extends CombinationType
    final case class TexasHoldemBoard() extends CombinationType
    final case class OmahaHoldemHand() extends CombinationType
    final case class OmahaHoldemBoard() extends CombinationType
    final case class MatchedCombination() extends CombinationType
  }

  final case class Combination private(cards: List[Card], ctype: CombinationType)
  object Combination {
    def apply(cards: List[Card], ctype: CombinationType): Either[CombinationError, Combination] = {
      val checkingSizeEq = (size: Int) => Either.cond(cards.length == size, new Combination(cards, ctype), IncorrectCombinationSize(s"must be $size cards"))

      ctype match {
        case CombinationType.OmahaHoldemBoard() => checkingSizeEq(5)
        case CombinationType.OmahaHoldemHand() => checkingSizeEq(4)
        case CombinationType.TexasHoldemBoard() => checkingSizeEq(5)
        case CombinationType.TexasHoldemHand() => checkingSizeEq(2)
        case CombinationType.MatchedCombination() => Either.cond(cards.length >= 2, new Combination(cards, ctype), IncorrectCombinationSize("must be at least 2 cards"))
        case _ => Left(UnsupportedCombination(ctype))
      }
    }
  }

  trait CombinationError
  final case class IncorrectCombinationSize(msg: String) extends CombinationError
  final case class UnsupportedCombination(ctype: CombinationType) extends CombinationError

  final case class TestCase(board: Combination, hands: List[Combination])
  final case class TestResult(board: Combination, hands: SortedSet[Combination])

}
