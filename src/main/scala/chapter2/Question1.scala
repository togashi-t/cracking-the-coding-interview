package chapter2

object Question1 extends App {


  // ソートされていない連結リストから重複する要素を排除
  // 2-1-1。再帰を使用する方法。
  // a :: (b :: (c :: Nil))のように、Nilに対して前方から重複していない要素を追加していく格好になる。
  def removeDuplicatedA[T](list: List[T]): List[T] = {

    def loop(currentList: List[T], seen: Set[T]): List[T] = {
      currentList match {
        case Nil => Nil
        case head :: tail =>
          if (seen.contains(head)) { // 重複する要素であった場合
            loop(tail, seen)
          } else { // 重複する要素ではなかった場合
            head :: loop(tail, seen + head)
          }
      }
    }

    loop(list, Set.empty[T])
  }


  // 2-1-2。foldLeftを使用する方法。
  def removeDuplicatedB[T](list: List[T]): List[T] = {
    val (outcomeList, _) = list.foldLeft((List.empty[T], Set.empty[T])) { case ((tmpList, tmpSeen), element) =>
      if (tmpSeen.contains(element)) { // 重複する要素の場合
        (tmpList, tmpSeen)
      } else { // 重複しない要素の場合
        (element :: tmpList, tmpSeen + element)
      }
    }

    outcomeList.reverse // 当初の順序とは逆になっているので元の順序に戻す
  }


  val sampleList = List(1, 2, 3, 4, 3, 2, 5, 6, 1)

  println("Testing removeDuplicatedA:")
  val resultA = removeDuplicatedA(sampleList)
  println(resultA.mkString(", "))

  println("Testing removeDuplicatedB:")
  val resultB = removeDuplicatedB(sampleList)
  println(resultB.mkString(", "))

  // 関数AとBの結果を比較
  assert(resultA == resultB, "Both methods should yield the same result.")
}
