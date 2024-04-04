package chapter2

object Question4 extends App {

  // リストの分割。ある数xが与えられたとき、リストの要素を並び替え、xより小さいものがxよりも前方にあるようにする。
  // 閾値を境にしてリストを分割した後結合する。
  // 2-4-1
  def partitionA(list: List[Int], x: Int): List[Int] = {
    val (lowers, uppers) = list.partition(_ < x)
    lowers ::: uppers
  }

  // 2-4-2
  def partitionB(list: List[Int], x: Int): List[Int] = {
    val (lowers, uppers) = list.foldLeft((List.empty[Int], List.empty[Int])) { case ((tmpLowers, tmpUppers), element) =>
      if (element < x) (element :: tmpLowers, tmpUppers) else (tmpLowers, element :: tmpUppers)
    }
    lowers ::: uppers
  }


  val testList = List(3, 5, 8, 5, 10, 2, 1)
  val res1 = partitionA(testList, 5)
  println(res1)
  val res2 = partitionB(testList, 5)
  println(res2)

}
