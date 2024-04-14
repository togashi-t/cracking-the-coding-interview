package chapter6

import scala.annotation.tailrec
import scala.util.Random

object Question7 extends App {

  // 黙示録:現在の文明が滅んだあとの新しい世界で、女王は出生率について心の底から心配していました。
  // そのため女王は、すべての家族は1人女の子を持つか、そうでなければ多額の罰金を支払わねばならないという法令を作りました。
  // すべての家族がこの規則を守るとすると、つまり女の子が生まれるまで子を持ち続け、生まれた時点で子供を増やすのをやめるとー新しい世代の男女比はどのようになるでしょうか?
  // (生まれる子どもが男である確率も女である確率も同じであると仮定してください。)
  // この問題について論理的に解答し、そのあとコンピュータシミュレーションを行ってください。


  // 女性が生まれるまで継続して子を持ち続けた場合の男性と女性の子の人数を返す。1つの家族について。
  def runOneFamily(random: Random): (Int, Int) = {
    @tailrec
    def simulate(boys: Int): (Int, Int) = {
      if (random.nextBoolean()) { // 生まれたのが女性の場合
        (boys, 1)
      } else {
        simulate(boys + 1)
      }
    }

    simulate(0)
  }

  // nの家族の合計の男性と女性の子の人数を返す
  def runNFamily(n: Int): (Int, Int) = {
    val random = new Random()

    (1 to n).foldLeft((0, 0)) { case ((accBoys, accGirls), _) =>
      val (boys, girls) = runOneFamily(random)
      (accBoys + boys, accGirls + girls)
    }
  }


  val (boys, girls) = runNFamily(100000000) // 1億の家族
  val res = girls.toDouble / (boys + girls)

  println(res)

}
