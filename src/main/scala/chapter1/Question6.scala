package chapter1

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Question6 extends App {

  // 1-6。文字列圧縮。元の文字列よりも短くならなかった場合は元の文字列を返す
  def compressStr(str: String): String = {
    val strLength = str.length
    // 圧縮結果を格納しているコレクション。圧縮結果が元のstrより短い場合のみ圧縮結果に意味があるため、その長さを設定。
    val compressedStrBuilder = new StringBuilder(strLength - 1)

    val maxIndex = strLength - 1

    @tailrec
    def loop(currentIndex: Int, currentCompressedLength: Int, consecutiveChar: Char, consecutiveCount: Int): String = {
      // まだ確定していない部分の圧縮文字列も含めて長さを算出
      val prospectCompressedLength = currentCompressedLength + (if (consecutiveCount == 1) 1 else (1 + countDigit(consecutiveCount)))
      if (prospectCompressedLength >= strLength) { // 元の文字列より短くない場合
        str
      } else if (currentIndex > maxIndex) { // 末尾の文字まで処理が完了した場合
        compressedStrBuilder.append(s"${consecutiveChar}${consecutiveCount}") // 連続した数を記録
        compressedStrBuilder.toString
      } else {
        val currentChar = str(currentIndex)
        if (currentChar == consecutiveChar) { // 文字の連続が継続している場合
          loop(currentIndex + 1, currentCompressedLength, consecutiveChar, consecutiveCount + 1)
        } else { // 文字の連続が絶たれた場合
          compressedStrBuilder.append(s"${consecutiveChar}${consecutiveCount}") // 連続した数を記録
          loop(
            currentIndex + 1,
            currentCompressedLength + (1 + countDigit(consecutiveCount)), // 文字の長さ＋数の長さ
            currentChar,
            1
          )
        }
      }
    }

    loop(1, 0, str(0), 1)
  }

  // 整数の桁数を求める。n.toString.lengthだとnの数が大きい場合にパフォーマンスが悪化するため。
  private def countDigit(n: Int): Int = {
    if (n == 0) 1
    else (math.log10(n.toDouble).pipe(math.floor) + 1).toInt // toDoubleを用いているのは、log10の引数の型はDoubleとする必要があるため。
  }



  val testCases = Seq(
    "aabcccccaaa", // 圧縮結果は "a2b1c5a3"
    "abcdef", // 圧縮しても短くならないので "abcdef"
    "aaaaa", // 圧縮結果は "a5"
    "bb", // 圧縮結果は "b2"
    "aabbcc" // 圧縮しても短くならないので "aabbcc"
  )

  testCases.foreach { str =>
    println(s"'$str' -> '${compressStr(str)}'")
  }


}
