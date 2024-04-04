package chapter2

object Question5 extends App {

  // 2-5
  // リストで表された2数の和:各ノードの要素が1桁の数である連結リストで表された2つの数があります。
  // 一の位がリストの先頭になるように、各位の数は逆順に並んでいます。このとき2つの数の和を求め、それを連結リストで表したものを返す関数を書いてください。
  // リストの長さは同じとは限らない
  // 例
  // 入力:(7->1->6)+(5->9->2)→617+295
  // 出力:2->1->9→912
  def sumReverseList(list1: List[Int], list2: List[Int]): List[Int] = {

    def loop(remainList1: List[Int], remainList2: List[Int], carry: Int): List[Int] = {
      (remainList1, remainList2) match {
        case (Nil, Nil) =>
          if (carry == 0) Nil else List(1)
        case (head1 :: tail1, Nil) =>
          val sum = head1 + carry
          genCurrentDigitValue(sum) :: loop(tail1, Nil, genCarry(sum))
        case (Nil, head2 :: tail2) =>
          val sum = head2 + carry
          genCurrentDigitValue(sum) :: loop(Nil, tail2, genCarry(sum))
        case (head1 :: tail1, head2 :: tail2) =>
          val sum = head1 + head2 + carry
          genCurrentDigitValue(sum) :: loop(tail1, tail2, genCarry(sum))
      }
    }

    def genCurrentDigitValue(sum: Int): Int = sum % 10

    def genCarry(sum: Int): Int = sum / 10


    loop(list1, list2, 0)
  }


  val testCases = List(
    (List(7, 1, 6), List(5, 9, 2)), // 617 + 295 = 912
    (List(0), List(0)), // 0 + 0 = 0
    (List(9, 9, 9), List(1)) // 999 + 1 = 1000
  )

  testCases.foreach { case (list1, list2) =>
    val result = sumReverseList(list1, list2)
    println(s"Sum of ${list1.reverse.mkString} and ${list2.reverse.mkString} is ${result.reverse.mkString}")
  }

}
