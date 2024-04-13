package chapter5

import scala.annotation.tailrec

object Question3 extends App {

  // ベストの反転位置:ある整数があり、その中の1ビットだけ0から1に反転することができます。
  // このような操作を行うとき、1の並びが最も長いときの長さを求めるコードを書いてください。
  // 例
  // 入力:1775(2進:11011101111)
  // 出力:8

  def bestInversionPosition(input: String): Int = {
    // 0または1の連続状況をハフマン符号化によりまとめる
    val huffmanList = input.foldLeft(List.empty[(Char, Int)]) { case (acc, char) =>
      acc match {
        case (lastChar, length) :: tail =>
          if (lastChar == char) { // 文字の連続が継続する場合
            (lastChar, length + 1) :: tail
          } else { // 文字の連続が途絶える場合
            (char, 1) :: acc
          }
        case _ => // inputの先頭要素処理時のみここに来る
          List((char, 1))
      }
    }

    @tailrec
    def getMaxLength(list: List[(Char, Int)], tmpMaxLength: Int): Int = list match {
      case ('1', len1) :: ('0', len2) :: ('1', len3) :: _ =>
        // 間の0の長さが1の場合のみ0から1の反転により1が一続きになる
        val updatedMaxLength = if (len2 == 1) math.max(len1 + 1 + len3, tmpMaxLength) else math.max(len1, len3) + 1
        // 1つスライドした場合('0', len1) :: ('1', len2) :: ('0', len3) :: restの連続最大値が上記より長くなり得ないので2つスライドする
        getMaxLength(list.tail.tail, updatedMaxLength)
      case ('0', _) :: ('1', len2) :: ('0', _) :: _ =>
        getMaxLength(list.tail, len2 + 1)
      case (char1, len1) :: (_, len2) :: Nil => // 残り要素数が2の場合。どちらかのcharが必ず1なので
        math.max((if (char1 == '1') len1 else len2) + 1, tmpMaxLength)
      case (char, len) :: Nil => // 残りの要素数が1の場合。
        math.max(if (char == '1') len else 0, tmpMaxLength)
      case _ =>
        math.max(0, tmpMaxLength)
    }


    getMaxLength(huffmanList, 0)
  }



  val res = bestInversionPosition("11011101111")
  println(res)

}
