package chapter5

object Question2 extends App {

  // 実数の2進表記:0から1までの実数値がdouble型として与えられるとき、それを2進表記で出力してください。
  // 32文字以内で正確に表現できない場合は"ERROR"と出力してください。 <- この要件は一旦断念。
  // 断念理由は、末尾再帰のコードであればこれを実現できるものの、その場合は以下いずれかの方法をとる必要があり、コストが高いため。
  // ・文字列として都度都度結合。その都度既存文字のコピーを伴う新しいオブジェクト生成が行われるので、処理が進んで文字列が長くなるほど時間計算量の点で問題がある。
  // ・リストに蓄積し、最後にrevereおよびmkString。reverseの時にListの長さに比例した時間計算量を要する。

  def convertToBinary(n: Double): Either[String, String] = {

    if (n >= 1 || n <= 0) {
      Left("ERROR")
    } else {
      // 各桁の表す10進数の数値を作成し、その数値でマイナスにならないように差し引けた場合、ビットが1ということ。
      def loop(rest: Double, divisor: Double, digitCount: Int): String = {
        if (rest == 0 || digitCount >= 33) {
          ""
        } else {
          val (bitNumStr, updatedRest) = if (rest >= divisor) ("1", rest - divisor) else ("0", rest)

          bitNumStr ++ loop(updatedRest, divisor / 2, digitCount + 1)
        }
      }

      Right("." ++ loop(n, 0.5, 1))
    }
  }


  val res = convertToBinary(0.625)
  println(res)

}
