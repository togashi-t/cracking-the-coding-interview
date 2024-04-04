package chapter2

import scala.annotation.tailrec

object Question2 extends App {


  // 後ろからK番目の要素を返す。2つのポインタを用いるランナーテクニックを使用。

  // 2-2-1
  // 1つ目のポインタが先行、2つ目のポイントは1つ目とはKの間隔を空ける。それぞれ同時に1つずつ進む。
  def getElementFromLast[T](list: List[T], k: Int): Option[T] = {
    val firstIterator = list.iterator.drop(k - 1)

    if (!firstIterator.hasNext) { // そもそもlistの長さがkよりも短い場合
      None
    } else {
      // listの先頭からk番目にいる
      firstIterator.next()
      // listの先頭の1つ手前にいる
      val secondIterator = list.iterator
      // 1つ目のポインタが末尾に到達するまで、両ポインタを同じ分だけ進める
      while (firstIterator.hasNext) {
        firstIterator.next()
        secondIterator.next()
      }

      secondIterator.nextOption()
    }
  }


  def getElementFromLastB[T](list: List[T], k: Int): Option[T] = {
    @tailrec
    def loop(leadList: List[T], followList: List[T]): Option[T] = {
      (leadList, followList) match {
        case (Nil, followHead :: _) => Some(followHead)
        case (_ :: leadTail, _ :: followTail) => loop(leadTail, followTail)
        case _ => None
      }
    }

    // リストの長さがk未満でないことを確認している
    list.drop(k - 1) match {
      case Nil => None
      case _ :: leadList => loop(leadList, list)
    }
  }



  val testList = List(1, 2, 3, 4, 5)
  println(getElementFromLastB(testList, 1))
  println(getElementFromLastB(testList, 3))
  println(getElementFromLastB(testList, 5))
  println(getElementFromLastB(testList, 6))




}
