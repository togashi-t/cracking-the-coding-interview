package chapter4

object Question12 extends App {

  // 合計が等しい経路:各ノードが整数値(正の場合も負の場合もあります)を持った二分木が与えられます。
  // このとき、与えられた値と合計値が等しくなるような経路を数えるアルゴリズムを設計してください。
  // 経路の始まりと終わりは必ずしも根と葉である必要はありませんが、下る方向への経路(親ノードから子ノードへの移動のみ)でなければなりません。

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)


  def countPathToSum(root: Option[Node], targetSum: Int): Int = {

    def helper(nodeOpt: Option[Node], currentSum: Int, sumToPathCountMap: Map[Int, Int]): Int = nodeOpt match {
      case None =>
        0
      case Some(node) =>
        // 新しい合計値
        val newSum = currentSum + node.value
        // 新しい合計値から目標値を差し引いた値
        val diff = newSum - targetSum
        // 目標値に到達するパスの数を計算。ルートではないどこかからの経路数＋ルートからの経路数
        val targetSumPathCount = sumToPathCountMap.getOrElse(diff, 0) + (if (newSum == targetSum) 1 else 0)

        // パスの合計値と出現回数のマップを更新
        val newSumToPathCountMap = sumToPathCountMap.updated(newSum, sumToPathCountMap.getOrElse(newSum, 0) + 1)

        // 左右の子ノードに対して再帰的に関数を呼び出し、パスの数を集計
        targetSumPathCount + helper(node.left, newSum, newSumToPathCountMap) + helper(node.right, newSum, newSumToPathCountMap)
    }

    helper(root, 0, Map.empty)
  }




  //        1
  //       / \
  //      2    3
  //     / \  / \
  //    4   5 6  7
  //   / \ /      \
  //  8  9 10     11
  //             /
  //            12




  val testTree = Node(1,
    Some(Node(2,
      Some(Node(4,
        Some(Node(8)),
        Some(Node(9))
      )),
      Some(Node(5,
        Some(Node(10)),
        None
      ))
    )),
    Some(Node(3,
      Some(Node(6)),
      Some(Node(7,
        None,
        Some(Node(11,
          Some(Node(12)),
          None
        ))
      ))
    ))
  )

  // 8になるのは、1 -> 2 -> 5, 8

  val res = countPathToSum(Some(testTree), 8)

  println(res)


}
