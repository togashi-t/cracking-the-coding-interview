package chapter7

object Question1 {

  // カードゲームのデッキ:一般的なカードゲームのデッキについてデータ構造を設計してください。
  // また、ブラックジャックをサブクラスとして実装するにはどのようにすればよいかを説明してください。

  // スーツ。クラブ、ダイヤ、ハート、スペードのいずれか。
  abstract class Suit
  object Suit {
    case object Club extends Suit
    case object Diamond extends Suit
    case object Heart extends Suit
    case object Spade extends Suit

    val values = List(Club, Diamond, Heart, Spade)
  }


  case class Card(suit: Suit, value: Int)

  val cards: List[Card] = for {
    s <- Suit.values
    value <- (1 to 13)
  } yield Card(s, value)



  // カードの山
  case class Deck(cards: List[Card])

  object Deck {
    def shuffle(deck: Deck): Deck = Deck(scala.util.Random.shuffle(cards))

    // デッキから1枚のカードを取り出す。取り出したカードと、新しいデッキを返す。
    def dealCard(deck: Deck): (Option[Card], Deck) = deck.cards match {
      case head :: tail => (Some(head), Deck(tail))
      case _ => (None, deck)
    }

    // デッキから指定された数のカードを取り出し、そのカードのリストと新しいデッキを返す。
    def dealHand(deck: Deck, number: Int): (List[Card], Deck) = {
      require(number >= 1)

      if (deck.cards.lengthIs >= number) {
        (deck.cards.take(number), Deck(deck.cards.drop(number)))
      } else {
        (Nil, deck)
      }
    }

  }


  // 手札
  case class Hand(cards: List[Card])
  object Hand {
    def addCard(card: Card): Hand = Hand(card :: cards)

    def addCard(cards_ : List[Card]): Hand = Hand(cards_ ::: cards)
  }

}
