package chapter1

import scala.annotation.tailrec

object Question1 extends App {

  // 1-1-1
  // 新たなデータ構造を使用し、そして文字コードがASCIIである場合。
  def isNoDuplicationStringA(str: String): Boolean = {
    val numberOfTypesCharacters = 128

    if (str.length > numberOfTypesCharacters) { // 文字種の数を超過している場合。必ず重複が存在する。
      false
    } else {
      // 各文字の存在数を記録するための配列
      val charSet = Array.fill(numberOfTypesCharacters)(0)

      @tailrec
      def loop(str: String, currentStrIndex: Int, maxStrIndex: Int): Boolean = {
        if (currentStrIndex > maxStrIndex) { // 全ての文字を走査し終えた場合
          true
        } else {
          val char = str(currentStrIndex)
          val asciiValue = char.toInt
          if (charSet(asciiValue) > 0) { // この文字の存在数記録を確認
            false
          } else {
            charSet(asciiValue) = 1
            loop(str, currentStrIndex + 1, maxStrIndex)
          }
        }
      }

      loop(str, 0, str.length - 1)
    }
  }


  // 1-1-2
  // 新たなデータ構造を使用しない場合
  def isNoDuplicationStringB(str: String): Boolean = {
    // 文字列の長さが1以下の場合は重複がないので
    if (str.length <= 1) {
      true
    } else {
      // 文字列を整列させる
      val sortedStr = str.sorted

      // 隣り合う文字列で同一のものがないか確認する。文字列の先頭から順番に2つの文字を取り出して確認。同一のものがあった時点でfalseとして終了。
      @tailrec
      def loop(sortedStr: String, currentHeadIndex: Int, maxHeadIndex: Int): Boolean = {
        if (currentHeadIndex > maxHeadIndex) { // 走査完了の場合
          true
        } else {
          if (sortedStr(currentHeadIndex) == sortedStr(currentHeadIndex + 1)) {
            false
          } else {
            loop(sortedStr, currentHeadIndex + 1, maxHeadIndex)
          }
        }
      }

      loop(sortedStr, 0, sortedStr.length - 2)
    }
  }



  println(isNoDuplicationStringB("abcedfg"))
  println(isNoDuplicationStringB("abccdfg"))
}
