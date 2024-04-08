package chapter4

object Question8 extends App {


  // 最初の共通祖先
  // 二分木において、2つのノードの上位ノードで最初に共通するものを探すアルゴリズムを設計。
  // ただし、データ構造の中に新たにノードを追加してはいけない(二分木は二分探索木とは限らない)。
  // また、ノードは親ノードへの参照を持たない。

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  def findFirstCommonAncestor(root: Option[Node], p: Int, q: Int): Option[Node] = {

    // 二分木を再帰的に探索
    // 各ステップで、探索対象のノードがpまたはqと等しいか、または左右の子からpとqの共通祖先を見つけられるかをチェック
    def helper(nodeOpt: Option[Node]): Option[Node] = nodeOpt match {
      case None => None
      case Some(node) =>
        if (node.value == p || node.value == q) {
          Some(node)
        } else {
          // 左右の子に対して再帰的に探索を行う
          val leftResult = helper(node.left)
          val rightResult = helper(node.right)

          (leftResult, rightResult) match {
            case (Some(_), Some(_)) => Some(node) // pとqが異なるサブツリーに位置する場合、現在のノードが共通祖先
            case (Some(_), None) => leftResult // 左のサブツリーに共通祖先またはp/qが見つかった場合
            case (None, Some(_)) => rightResult // 右のサブツリーに共通祖先またはp/qが見つかった場合
            case _ => None // 共通祖先が見つからない場合
          }
        }
    }

    helper(root)
  }

  // treeの例

  //       3
  //      / \
  //     5   1
  //    / \ / \
  //   6  2 0  8
  //     / \
  //    7   4


  // リーフノードの作成
  val node7 = Node(7)
  val node4 = Node(4)
  val node6 = Node(6)
  val node0 = Node(0)
  val node8 = Node(8)

  // 中間ノードの作成
  val node2 = Node(2, left = Some(node7), right = Some(node4))
  val node5 = Node(5, left = Some(node6), right = Some(node2))
  val node1 = Node(1, left = Some(node0), right = Some(node8))

  // ルートノードの作成
  val root = Node(3, left = Some(node5), right = Some(node1))



  println(findFirstCommonAncestor(Some(root), 4, 6).map(_.value))

}
