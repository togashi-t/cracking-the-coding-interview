package chapter5

object Question4 extends App {

  // 隣の数:正の整数が与えられたとき、2進表現したときの1の個数が同じ整数の中で、1つ後の数と前の数を求めてください。


  def getNext(n: Int): Int = {
    val count0 = countTailingZeros(n)
    val count1 = countTailingOnes(n >> count0)
    
    n + (1 << count0) + (1 << (count1 - 1)) - 1
  }


  def getPrev(n: Int): Int = {
    val count1 = countTailingOnes(n)
    val count0 = countTailingZeros(n >> count1)

    n - (1 << count1) - (1 << (count0 - 1)) + 1
  }


  // 数値を2進数で表現した場合における末尾の
  // 0の数を数える
  def countTailingZeros(n: Int): Int = Integer.numberOfTrailingZeros(n)
  // 1の数を数える
  def countTailingOnes(n: Int): Int = Integer.numberOfTrailingZeros(~n)


}
