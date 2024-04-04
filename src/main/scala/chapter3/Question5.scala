package chapter3

import scala.annotation.tailrec

object Question5 extends App {

  // 3-5。スタックのソート。
  // 最も小さい項目がトップに来るスタックを並び替えるプログラムを書く。別のスタックを1つ用意してもOK。
  // 実際的にはリストの並び替えと同義なので、リストの並び替えを実装する。
  // 別のリストに降順での並び替えが完了がゴール。

  // 3-5-1
  def sortListA(list: List[Int]): List[Int] = {
    /**
     * 並び替えを行う処理。処理概要は以下。
     * ・mainListからsubListに値を1つずつ移していく。
     * ・この時降順になるようにしたいので、subList先頭値未満の場合は以下を行う。
     * 　・pendingOptに値を保持
     * 　・subListからmainListに値を1つずつ移していく。
     * 　・上記の際、subList先頭値がpendingの値以下となったら、pendingの値をsubListに追加する。
     *
     * @param mainList
     * @param subList
     * @param pendingOpt
     */

    @tailrec
    def sort(mainList: List[Int], subList: List[Int], pendingOpt: Option[Int]): List[Int] = {
      if (mainList.isEmpty && subList.isEmpty) { // そもそも並び替え対象のリストが空の場合はここに来る
        Nil
      } else {
        pendingOpt match {
          case None => // mainListからsubListに値を移す
            mainList match {
              case mainHead :: mainTail =>
                subList match {
                  case subHead :: subTail if subHead > mainHead => // 移すとsubListに降順にならない場合
                    sort(subHead :: mainTail, subTail, Some(mainHead))
                  case _ =>
                    sort(mainTail, mainHead :: subList, None)
                }
              case Nil => // 全てsubListに移し終えた場合
                subList
            }
          case Some(pending) => // pendingとの大小関係を確認の上で、subListからmainListに値を移す
            subList match {
              case subHead :: subTail =>
                if (pending >= subHead) { // pending終了の場合
                  sort(mainList, pending :: subList, None)
                } else { // pending継続の場合
                  sort(subHead :: mainList, subTail, Some(pending))
                }
              case Nil =>
                sort(mainList, List(pending), None)
            }
        }
      }
    }

    sort(list, Nil, None)
  }


  // 3-5-2。再帰を使用。末尾再帰にできない部分があるので、データ量が多すぎるとスタックオーバーフローが発生する
  def sortListB(list: List[Int]): List[Int] = {

    // elementをsortedList(降順)に追加する。降順を維持するかたちで。返すのは、elementを追加した後の降順のリスト
    def insertIntoSortedList(element: Int, sortedList: List[Int]): List[Int] = {
      sortedList match {
        case head :: tail =>
          if (element >= head) element :: sortedList else head :: insertIntoSortedList(element, tail)
        case Nil =>
          List(element)
      }
    }

    @tailrec
    def sort(mainList: List[Int], sortedList: List[Int]): List[Int] = {
      mainList match {
        case head :: tail =>
          val updatedSortedList = insertIntoSortedList(head, sortedList)
          sort(tail, updatedSortedList)
        case Nil =>
          sortedList
      }
    }

    sort(list, Nil)
  }


  // 3-5-3。末尾再帰を使用。設問の制約に反するが、リストを2つ使用する。
  def sortListC(list: List[Int]): List[Int] = {

    /**
     * 要素(element)をsortedList(降順)に追加する。降順を維持するかたちで。返すのは、elementを追加した後の降順のリスト
     *
     * @param element
     * @param sortedList
     * @param shelterList 待避用のリスト。sortedListから取り出した要素を積むためのもの。
     * @return
     */
    @tailrec
    def insertIntoSortedList(element: Int, sortedList: List[Int], shelterList: List[Int]): List[Int] = {
      sortedList match {
        case head :: tail if element < head => // 要素をそのままsortedListへ追加するとsortされていないことになる場合
          insertIntoSortedList(element, tail, head :: shelterList)
        case _ => // sortedListが空になった場合、またはelementが現在のhead以上の場合
          // shelterListの要素を反転させてelementを追加し、残りのsortedListを連結する
          shelterList.reverse ::: (element :: sortedList)
      }
    }

    @tailrec
    def sort(mainList: List[Int], sortedList: List[Int]): List[Int] = {
      mainList match {
        case head :: tail =>
          val updatedSortedList = insertIntoSortedList(head, sortedList, Nil)
          sort(tail, updatedSortedList)
        case Nil =>
          sortedList
      }
    }

    sort(list, Nil)
  }




  val listsToSort = List(
    List(3, 2, 1),
    List(4, 3, 5, 1, 2),
    List(1, 2, 3, 4, 5),
    List(5, 4, 3, 2, 1),
    List(),
    List(1),
    List(1, -1, 0, 2, -2)
  )

  // 各リストに対してsortListC関数を適用し、結果を表示
  listsToSort.foreach { list =>
    println(s"Original: $list, Sorted: ${sortListC(list)}")
  }

}
