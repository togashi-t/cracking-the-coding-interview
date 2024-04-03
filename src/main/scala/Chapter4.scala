import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Chapter4 extends App {

  // 4-1
  // 有向グラフが与えられたとき、2つのノード間に経路があるかどうかを判定するアルゴリズムを設計する。
  // ノードから構成されるグラフを、Mapで表現する。keyがノードの値、valueが当該ノードから移動できる隣接ノードの値のリスト。
  def isPathPresentBFS(graph: Map[Int, List[Int]], start: Int, end: Int): Boolean = {

    /***
     * 探索して到達可能かを返す
     * @param toVisitQueue 訪問すべきノード。Queueとした理由は先頭から探索したい、および後方に新たな探索先追加したいため。
     * @param visitedSet 訪問済のノード。何度も同じノードに訪問する
     * @return
     */
    @tailrec
    def loop(toVisitQueue: collection.immutable.Queue[Int], visitedSet: Set[Int]): Boolean = {
      toVisitQueue.dequeueOption match {
        case None => // 訪問すべきノードがもうない場合
          false
        case Some((head, tail)) =>
          if (head == end) { // 訪問ノードが目的地ノードの場合
            true
          } else {
            // 現在のノードを訪問済に追加
            val updatedVisitedSet = visitedSet + head
            // 現在のノードから到達可能な隣接ノードを取得し、そこから訪問済を除く
            val neighborList = graph.getOrElse(head, Nil).filterNot(updatedVisitedSet.contains)
            // 探索を継続
            loop(tail.enqueueAll(neighborList), updatedVisitedSet)
          }
      }
    }

    loop(collection.immutable.Queue(start), Set.empty[Int])
  }


  val graph: Map[Int, List[Int]] = Map(
    1 -> List(2, 3),
    2 -> List(4),
    3 -> List(5),
    4 -> List(6),
    5 -> List(),
    6 -> List(3) // このエッジにより、3から4への間接的な経路が存在する
  )

  // テストケース1: 経路が存在する場合
  println("Test 1: Should find a path from 1 to 6")
  println(isPathPresentBFS(graph, 1, 6)) // trueを期待

  // テストケース2: 経路が存在しない場合
  println("Test 2: Should not find a path from 5 to 1")
  println(isPathPresentBFS(graph, 5, 1)) // falseを期待

  // テストケース3: 開始ノードと終了ノードが同じ場合
  println("Test 3: Should find a path from 3 to 3 (the same node)")
  println(isPathPresentBFS(graph, 3, 3)) // trueを期待

  // テストケース4: 間接的な経路が存在する場合
  println("Test 4: Should find a path from 1 to 5 through an indirect route")
  println(isPathPresentBFS(graph, 1, 5)) // trueを期待












}
