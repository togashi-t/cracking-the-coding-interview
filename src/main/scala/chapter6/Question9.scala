package chapter6

import scala.annotation.tailrec

object Question9 extends App {

  // 100個のロッカー:廊下に扉のしまったロッカーが100個あります。
  // まず、100のロッカーのすべての扉を間に(1つ飛びで)扉を閉めていきます。
  // 今度は3つごとに(2つ飛びで)扉が開いていれば閉めてけていきます。
  // このようなことを繰り返していくと、100回目に100番目のロッカーの開け閉めをしての時点で、開いている扉はいくつありますか?



  //解法
  //扉の開け閉めが何を意味しているのかをよく考えることで、この問題に取り組んでいきます。

  //疑問その1:扉が開け閉めされるのはいつ?
  //n番目の扉が開け閉めされるのはnの約数と対応しています。たとえば15番目のロッカーなら、1、3、5、15回目に開け閉めされます。

  //疑問その2:開けたままになっている扉は?
  //開けたままになっているのは、扉番号の約数が奇数個になっているときです。約数2つを開閉の1セットと考えればわかりますね?

  //疑問その3:どんなときに奇数個になる?
  //ある数nに対して約数の個数が奇数個になるのは、nが2乗の数のときです。これはnの約数の中で、かけてnになる数のベアを作っていけばわかります。
  //たとえばnが36だとすると、かけて36になる約数のベアは(1,36)、(2,18)、(3,12)、(4,9)、(6,6)ですが、(6,6)は同じ約数になっていますから1つ省かねばなりません。
  //そのため、約数の個数は奇数個になるのです。

  //疑問その4:2乗の数はいくつある?
  //1、4、9、16、25、36、49、64、81、100と数えていけば10個あることがわかります。単純に1から順に2乗の数を作ってもわかりますね。
  //1*1,2*2,3*3,..., 10*10
  //というわけで、10個のロッカーが最終的に開いているということになります。



  // 愚直にプログラムで導出しようとすると以下
  def openOrClose(lockerCount: Int): Array[Boolean] = {
    // 各ロッカーの開閉状況を管理する配列作成。配列のindexとロッカーの番号が合致していた方が楽なので+1している。
    val arr = Array.fill(lockerCount + 1)(false)

    // 全ての約数で順にロッカーの開閉を行う。
    // 全ての約数での開閉が完了した後の配列を返す。
    @tailrec
    def openOrCloseImpl(divisor: Int, index: Int): Array[Boolean] = {
      if (divisor > lockerCount) { // 全ての約数での処理が終わった場合
        arr
      } else if (index > lockerCount) { // 現在の約数での処理が終わった場合。約数をインクリメント。
        val newDivisor = divisor + 1
        openOrCloseImpl(newDivisor, newDivisor)
      } else { // ロッカーの開閉を行い、次のロッカーへ。
        arr(index) = !arr(index)
        openOrCloseImpl(divisor, index + divisor)
      }
    }

    openOrCloseImpl(1, 1)
  }


  openOrClose(100).zipWithIndex.collect { case (bool, index) if bool => index }.foreach(println)


}
