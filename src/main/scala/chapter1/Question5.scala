package chapter1

import scala.annotation.tailrec

object Question5 extends App {


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



  val testCases = Seq(
    ("pale", "ple"), // true
    ("pales", "pale"), // true
    ("pale", "bale"), // true
    ("pale", "bake"), // false
  )

  testCases.foreach { case (a, b) =>
    println(s"(${a}, ${b}) -> ${isConvertable(a, b)}")
  }

}
