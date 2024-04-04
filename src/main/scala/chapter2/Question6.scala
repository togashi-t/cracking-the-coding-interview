package chapter2

import scala.annotation.tailrec

object Question6 extends App {

  // 回文であるかの判定
  // 2-6-1。リストを反転させて、元のリストと比べる。
  def isPalindromeA[T](list: List[T]): Boolean = {
    @tailrec
    def loop(remainNormalList: List[T], remainReverseList: List[T]): Boolean = {
      (remainNormalList, remainReverseList) match {
        case (normalHead :: normalTail, reverseHead :: reverseTail) =>
          if (normalHead == reverseHead) loop(normalTail, reverseTail) else false
        case _ => // 最後の要素まで突合が終了した場合
          true
      }
    }

    loop(list, list.reverse)
  }


  // 2-6-2。ランナーテクニックを使用。処理概要は以下。
  // ・先頭から、一方ではリストの参照範囲を1ずつ狭めていき、もう一方では2ずつ狭めていく。
  // ・1ずつ狭める際に要素を専用のリストに格納していく。(A)
  // ・2ずつ狭めたものがリストの末尾に到達。1ずつ狭めたものはリストの真ん中に到達。(B)
  // ・(A)と(B)の残りが合致するか確認
  def isPalindromeB[T](list: List[T]): Boolean = {
    @tailrec
    def loop(restSlowList: List[T], restFastList: List[T], prevReverseList: List[T]): Boolean = {
      restFastList match {
        case Nil => // リストの長さが偶数
          restSlowList == prevReverseList
        case _ :: Nil => // リストの長さが奇数
          restSlowList.tail == prevReverseList
        case _ :: _ :: updatedRestFastList => // 終端まではまだ遠くリストの長さ不明
          loop(restSlowList.tail, updatedRestFastList, restSlowList.head :: prevReverseList)
      }
    }

    loop(list, list, List.empty[T])
  }


  val testCases = List(
    List(1, 2, 3, 2, 1),
    List("a", "b", "b", "a"),
    "apple".toList,
    List(1, 2, 3, 4, 5),
    List[Int]()
  )

  testCases.foreach { list =>
    val result = isPalindromeB(list)
    println(s"List: ${list.mkString(", ")} is palindrome: $result")
  }


}
