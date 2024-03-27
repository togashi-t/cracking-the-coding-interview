import scala.annotation.tailrec

object Chapter2 extends App {

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


  // 2-2。後ろからK番目の要素を返す。2つのポインタを用いるランナーテクニックを使用。
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
          (if (sum >= 10) sum - 10 else sum) :: loop(tail1, Nil, if (sum >= 10) 1 else 0)
        case (Nil, head2 :: tail2) =>
          val sum = head2 + carry
          (if (sum >= 10) sum - 10 else sum) :: loop(Nil, tail2, if (sum >= 10) 1 else 0)
        case (head1 :: tail1, head2 :: tail2) =>
          val sum = head1 + head2 + carry
          (if (sum >= 10) sum - 10 else sum) :: loop(tail1, tail2, if (sum >= 10) 1 else 0)
      }
    }

    loop(list1, list2, 0)
  }


  // 2-6。回文であるかの判定
  def isPalindrome[T](list: List[T]): Boolean = {
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


  println(isPalindrome("abcdedcba".toList))














}
