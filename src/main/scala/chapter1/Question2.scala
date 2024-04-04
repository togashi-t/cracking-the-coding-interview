package chapter1

import scala.annotation.tailrec

object Question2 extends App {


  // 1-2-0。単純に真逆に並んでいることを確認する（書籍の問題の意味はこれではなかった）
  def isReverse(strA: String, strB: String): Boolean = {
    // そもそも長さが異なる場合はその時点でfalse
    val lengthOfA = strA.length
    val lengthOfB = strB.length

    if (lengthOfA != lengthOfB) {
      false
    } else {
      // 一方の要素は先頭から、もう一方の要素は末尾から要素を取得し、同一であるか確認する。
      @tailrec
      def loop(strA: String, strB: String, currentIndex: Int, lastIndex: Int): Boolean = {
        if (currentIndex > lastIndex) {
          true
        } else {
          if (strA(currentIndex) != strB(lastIndex - currentIndex)) {
            false
          } else {
            loop(strA, strB, currentIndex + 1, lastIndex)
          }
        }
      }

      loop(strA, strB, 0, lengthOfA - 1)
    }
  }


  // 1-2。2つの文字列が同じ文字で構成されているか。
  def isSameCharacters(strA: String, strB: String): Boolean = {
    if (strA.length != strB.length) {
      false
    } else {
      // 2つの文字を構成している文字の数をまとめる
      val strACountMap = strA.groupBy(identity).view.mapValues(_.length).toMap
      val strBCountMap = strB.groupBy(identity).view.mapValues(_.length).toMap

      strACountMap == strBCountMap
    }
  }


  val testCases = Seq(
    ("abc", "cba"),
    ("abc", "cbad"),
    ("aabbcc", "abcabc"),
    ("abc", "abz"),
    ("", "")
  )

  testCases.foreach { case (strA, strB) =>
    println(s"isSameCharacters('$strA', '$strB'): ${isSameCharacters(strA, strB)}")
  }
}
