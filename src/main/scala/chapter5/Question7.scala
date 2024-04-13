package chapter5

object Question7 extends App {

  // ビット・ペアの交換:偶数ビットと奇数ビットを、できるだけ少ない操作で入れ替えるプログラムを書いてください
  // (たとえば、0ビット目と1ビット目、2ビット目と3ビット目を入れ替えます)。



  // 偶数ビットを選択するマスクと奇数日ビットを選択するマスクを用意
  // 偶数ビットを右にシフト
  // 奇数ビットを左にシフト
  // 上記のOR演算を行う
  def swapOddEvenBits(n: Int): Int = {
    val evenMask = 0xAAAAAAAA // 10101010 10101010 10101010 10101010
    val oddMask = 0x55555555 // 01010101 01010101 01010101 01010101

    ((n & evenMask) >> 1) | (n & oddMask << 1)
  }

  println(13.toBinaryString)

  val res = swapOddEvenBits(13)
  println(res.toBinaryString)

}
