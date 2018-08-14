import scala.util.Random

case class Card(suit: String, rank: String)

case class Deck(cards: List[Card])

case class Player(name: String, deck: Deck)

object CardGameWar {
  // Feel free to use these cards or use your own data structure
  val suits = List("Spade", "Club", "Diamond", "Heart")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")

  val suitsRank = suits.zipWithIndex.toMap
  val numberRank = ranks.zipWithIndex.toMap

  // Creates two shuffled decks of cards
  def createDecks: (Deck, Deck) = {
    val allCards =
      new Random shuffle (for {
        suit <- suits
        rank <- ranks
      } yield Card(suit, rank.toString))

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    (player1, player2) match {
      case (a, b) if numberRank(a.rank) > numberRank(b.rank) => a
      case (a, b) if numberRank(a.rank) < numberRank(b.rank) => b
      case (a, b) if numberRank(a.rank) == numberRank(b.rank) && suitsRank(a.suit) < suitsRank(b.suit) => b
      case (a, b) if numberRank(a.rank) == numberRank(b.rank) && suitsRank(a.suit) > suitsRank(b.suit) => a
    }
  }

  def playGame(player1: Player, player2: Player): String = {
    (player1, player2) match {
      case (a, b) if a.deck.cards.isEmpty => b.name
      case (a, b) if b.deck.cards.isEmpty => a.name
      case (a, b) if playRound(a.deck.cards.head, b.deck.cards.head) == a.deck.cards.head => playGame(Player(a.name, Deck(a.deck.cards.tail ++ List(a.deck.cards.head, b.deck.cards.head))), Player(b.name, Deck(b.deck.cards.tail)))
      case (a, b) if playRound(a.deck.cards.head, b.deck.cards.head) == b.deck.cards.head => playGame(Player(a.name, Deck(a.deck.cards.tail)), Player(b.name, Deck(b.deck.cards.tail ++ List(b.deck.cards.head, a.deck.cards.head))))
    }
  }

}

