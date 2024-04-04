package chapter2

import scala.annotation.tailrec

object Question7 extends App {

  // 2-7
  // 2つのリストの最初の交差点の値を返す。
  // 交差がある場合は、交差開始時の要素の値をOptionで返す。交差しない場合はNoneを返す。
  // Scalaのリストは交差し得ないので、疑似的に値が同じ事を交差とみなして実装する。
  def getFirstCrossNode[T](list1: List[T], list2: List[T]): Option[T] = {
    (list1, list2) match {
      case (list1Head :: list1Tail, list2Head :: list2Tail) =>

        // 両リストの末尾の値が異なる場合は交差していない。したがって両リストの末尾の値を調べる。その過程で両リストの長さも確認する。
        def getLastAndLength(head: T, tail: List[T]): (T, Int) = {
          tail.foldLeft((head, 1)) { case ((_, tmpCount), element) =>
            (element, tmpCount + 1)
          }
        }

        val (list1Last, list1Length) = getLastAndLength(list1Head, list1Tail)
        val (list2Last, list2Length) = getLastAndLength(list2Head, list2Tail)

        if (list1Last != list2Last) {
          None
        } else {
          // 最初の同一要素の値を返す
          @tailrec
          def getFirstCommonValue(listA: List[T], listB: List[T]): Option[T] = {
            if (listA.headOption == listB.headOption) listA.headOption else getFirstCommonValue(listA.tail, listB.tail)
          }

          // リストの長さを同一にする
          val lengthDiff = list1Length - list2Length
          if (lengthDiff >= 0) getFirstCommonValue(list1.drop(lengthDiff), list2) else getFirstCommonValue(list1, list2.drop(-lengthDiff))
        }
      case _ => // 両リストのいずれかでも空の場合は交差し得ない
        None
    }
  }


  println(getFirstCrossNode("1234567".toList, "567".toList))

}
