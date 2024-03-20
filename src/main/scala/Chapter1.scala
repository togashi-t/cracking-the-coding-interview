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
      def go(str: String, currentStrIndex: Int, maxStrIndex: Int): Boolean = {
        if (currentStrIndex > maxStrIndex) { // 全ての文字を走査し終えた場合
          true
        } else {
          val char = str(currentStrIndex)
          val asciiValue = char.toInt
          if (charSet(asciiValue) > 0) { // この文字の存在数記録を確認
            false
          } else {
            charSet(asciiValue) = 1
            go(str, currentStrIndex + 1, maxStrIndex)
          }
        }
      }

      go(str, 0, str.length - 1)
    }
  }


  println(isNoDuplicationStringA("abcあ"))

}
