package chapter5

object Question1 extends App {

  // 挿入:最大32ビットの整数NとM、ビットの位置を指す値iとjが与えられています。
  // このとき、"のjビット目からiビット目にMを挿入するメソッドを書いてください。
  // ただし、jとiの幅はMのビット数と一致していると仮定してかまいません。
  // つまり、M=10011であればjとiの幅は5と仮定してかまいません。j=3、i=2のような、Mの幅と合わないような場合は考えなくてもかまわないということです。
  //例
  //入力:N=10000000000,M=10011,i =2, j= 6
  //出力:N=10001001100

  def insertBits(n: Int, m: Int, i: Int, j: Int): Int = {
    // 挿入部分をクリアするためのmaskを作成
    // 111111111.....(j-i+1個の0)(i個の1)
    val mask = {
      // 1111111.....の後に0がj+1個
      val left = ~0 << (j + 1)
      // i個の1
      val right = (1 << i) - 1

      left | right
    }

    // 挿入部分の桁を全て0にしたもの | mについて挿入位置を合わせたもの
    (n & mask) | (m << i)
  }


  val n = Integer.parseInt("10000000000", 2)
  val m = Integer.parseInt("1011", 2)
  val i = 3
  val j = 6
  val res = insertBits(n, m, i, j)

  // 出力結果を2進数の文字列として得たいので、.toBinaryStringを使用している。
  println(res.toBinaryString)

}
