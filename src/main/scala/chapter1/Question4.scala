package chapter1

object Question4 extends App {

  // 文字列を並び替えることによって回文作成が可能かを返す。
  // 1-4-1。数を数える方法。
  def isRearrangeableToPalindromeA(str: String): Boolean = {
    val charCountMap = str
      .replaceAll(" ", "") // スペースは考慮の対象外なので除外
      .toLowerCase // 大文字小文字の区別はしないので全て小文字で処理する
      .groupBy(identity).view.mapValues(_.length).toMap // 文字毎の出現回数を集計
    // 使用回数が奇数回の文字が1個以下の場合は回文作成が可能
    charCountMap.values.count(_ % 2 == 1) <= 1
  }

  // 1-4-2。文字種類毎にtoggleする方法
  def isRearrangeableToPalindromeB(str: String): Boolean = {
    val oddNumberCharSet = scala.collection.mutable.Set.empty[Char]
    val lowerCaseStr = str
      .replaceAll(" ", "") // スペースは考慮の対象外なので除外
      .toLowerCase // 大文字小文字の区別はしないので全て小文字で処理する
    // 文字の出現ごとにSetの中の当該文字をtoggleする
    for (char <- lowerCaseStr) {
      if (oddNumberCharSet.contains(char)) oddNumberCharSet -= char else oddNumberCharSet += char
    }

    // 使用回数が奇数回の文字が1個以下の場合は回文作成が可能
    oddNumberCharSet.size <= 1
  }



  val testCases = Seq(
    "Tact Coa", // True: "taco cat", "atco cta" など
    "racecar", // True: 自身が回文
    "python", // False: 回文不可
    " ", // True: 空文字列またはスペースのみ
    "A man a plan a canal Panama" // True: 回文フレーズ
  )

  testCases.foreach { str =>
    println(s"'$str' -> ${isRearrangeableToPalindromeB(str)}")
  }

}
