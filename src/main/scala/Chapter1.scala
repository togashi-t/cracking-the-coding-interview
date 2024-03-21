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


  // 1-5。一発変換可否。2つの文字列が与えられたとき、一方の文字列に対して1操作（文字の挿入or削除or置換）（または操作なし）でもう一方の文字列に変換できるか。
  def isConvertable(strA: String, strB: String): Boolean = {
    val strALength = strA.length
    val strBLength = strB.length

    if (scala.math.abs(strALength - strBLength) >= 2) { // 文字列の長さの差が2以上の場合は不可
      false
    } else if (strALength == strBLength) { // 文字列の長さが同一の場合は、相違している文字数が1以下の場合は可math.abs
      @tailrec
      def loop(strA: String, strB: String, currentIndex: Int, maxIndex: Int, diffCount: Int): Boolean = {
        if (currentIndex > maxIndex) { // 全ての捜査が完了の場合
          diffCount <= 1
        } else if (diffCount > 1) { // 既に違う文字数が1より大きい場合
          false
        } else {
          loop(strA, strB, currentIndex + 1, maxIndex, if (strA(currentIndex) == strB(currentIndex)) diffCount else diffCount + 1)
        }
      }

      loop(strA, strB, 0, strALength - 1, 0)
    } else { // 文字列の長さが1だけ異なる場合
      // 短い方の文字列に1つの文字を追加して長い文字列となるかを確認する。
      // 具体的な方法は、
      // (1)両文字列のindex対象文字が同じであるか比較。初めのindexは両方共0
      // (2)もし同じ場合は両indexに+1。もし異なる場合は長い文字のみindexを+1する。
      // (1)(2)を繰り返すが、indexが最後まで到達したらtrue、その前にindexの差が2となったらfalse。
      val (shorterStr, longerStr) = if (strALength < strBLength) (strA, strB) else (strB, strA)
      val (shorterStrMaxIndex, longerStrMaxIndex) = (scala.math.min(strALength, strBLength) - 1, scala.math.max(strALength, strBLength) - 1)

      @tailrec
      def loop(shorterStrCurrentIndex: Int, longerStrCurrentIndex: Int): Boolean = {
        if (shorterStrCurrentIndex <= shorterStrMaxIndex && longerStrCurrentIndex <= longerStrMaxIndex) { // まだ最後の文字に到達していない場合。
          if (shorterStr(shorterStrCurrentIndex) == longerStr(longerStrCurrentIndex)) { // 文字が一致した場合
            loop(shorterStrCurrentIndex + 1, longerStrCurrentIndex + 1) // 参照するindexを両方共進める
          } else if (shorterStrCurrentIndex != longerStrCurrentIndex) { // 2回目の文字不一致
            false
          } else { // 1回目の不一致。長い方の文字のindexを1つずらす。
            loop(shorterStrCurrentIndex, longerStrCurrentIndex + 1)
          }
        } else {
          true
        }
      }

      loop(0, 0)
    }
  }





  println(isConvertable("pale", "ple"))
  println(isConvertable("pales", "pale"))
  println(isConvertable("pale", "bale"))
  println(isConvertable("pale", "bake"))






}
