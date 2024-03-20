import scala.annotation.tailrec

object Chapter1 extends App {

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


  // 1-3。空白文字を指定文字で置換。ただし、strについては指定された長さ以降は切り捨てる。
  def replaceAllSpace(str: String, trueLength: Int): String = {
    str.take(trueLength).replaceAll(" ", "%20")
  }




  println(replaceAllSpace("Mr John Smith ", 13))






}
