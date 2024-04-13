package chapter5

import scala.annotation.tailrec

object Question6 extends App {

  // ビット変換:ある整数AからBに変換するのに必要なビット数を決定する関数を書いてください。
  // 例
  // 入力:29(または:11101),15(または:01111)
  // 出力:2

  // AとBのXOR演算を行い、異なるビットを1とする。
  // 上記結果の1のビットを数える。
  def convertBitCount(a: Int, b: Int): Int = {
    val diff = a ^ b

    @tailrec
    def countDiffBit(x: Int, acc: Int): Int = {
      if (x == 0) acc else countDiffBit(x >> 1, acc + (x & 1))
    }

    countDiffBit(diff, 0)

    // Integer.bitCount(a ^ b)
  }



  println(convertBitCount(29, 15))



}
