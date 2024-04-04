import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Chapter4 extends App {

  // 4-1
  // 有向グラフが与えられたとき、2つのノード間に経路があるかどうかを判定するアルゴリズムを設計する。
  // ノードから構成されるグラフを、Mapで表現する。keyがノードの値、valueが当該ノードから移動できる隣接ノードの値のリスト。

  // 幅優先探索と深さ優先探索のコードの違いは、新たに見つけた訪問先の訪問優先順位を以下として取り扱うこと。
  // ・幅優先探索：最低
  // ・深さ優先探索：最高

  // 4-1-1。幅優先探索
  // 最短経路を見つけるのに適している。最初に見つかった経路が、開始ノードと目的ノード間の最短経路。
  // 幅が広いグラフでは、探索範囲が大きくなり、比較的多くのメモリを消費する可能性がある。
  // 現実世界での使用実例。
  // ・最短経路問題: ソーシャルネットワーク上で2人のユーザー間の最短の友人関係の経路を見つける場合。迷路の解を探す場合。
  // ・ネットワークブロードキャスト: 最も近いノードから順に情報を伝えることで、効率的に全ノードに情報を広めることができる。
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


  // 4-1-2。深さ優先探索
  // 最短経路を保証しない。
  // スタックには一度に一つの探索パス上のノードのみが保持されるため、BFSで多数のノードを保持するのと比較してメモリ使用量が少なくなる場合がある。
  // 現実世界での使用実例。
  // ・解の深さが不明で全ての可能性を探索:
  // 　・迷路の全経路探索
  // 　・チェスや将棋。特定のゲーム状態から可能な全ての手を深く探索し、勝利につながる手順を見つける。
  def isPathPresentDFS(graph: Map[Int, List[Int]], start: Int, end: Int): Boolean = {
    @tailrec
    def loop(toVisitList: List[Int], visitedSet: Set[Int]): Boolean = {
      toVisitList match {
        case Nil => // 訪問すべきノードがもうない場合
          false
        case head :: tail =>
          if (head == end) { // 訪問ノードが目的地ノードの場合
            true
          } else {
            // 現在のノードを訪問済に追加
            val updatedVisitedSet = visitedSet + head
            // 現在のノードから到達可能な隣接ノードを取得し、そこから訪問済を除く
            val neighborList = graph.getOrElse(head, Nil).filterNot(updatedVisitedSet.contains)
            // 探索を継続
            loop(neighborList ::: tail, updatedVisitedSet)
          }
      }
    }

    loop(List(start), Set.empty[Int])
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
  println(isPathPresentDFS(graph, 1, 6)) // trueを期待


  // テストケース2: 経路が存在しない場合
  println("Test 2: Should not find a path from 5 to 1")
  println(isPathPresentDFS(graph, 5, 1)) // falseを期待

  // テストケース3: 開始ノードと終了ノードが同じ場合
  println("Test 3: Should find a path from 3 to 3 (the same node)")
  println(isPathPresentDFS(graph, 3, 3)) // trueを期待

  // テストケース4: 間接的な経路が存在する場合
  println("Test 4: Should find a path from 1 to 5 through an indirect route")
  println(isPathPresentDFS(graph, 1, 5)) // trueを期待












}
