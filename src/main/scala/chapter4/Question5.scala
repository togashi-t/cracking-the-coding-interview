package chapter4

object Question5 extends App {

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  // 二分木が二分探索木であるかどうかを調べる関数を実装
  def isBST(rootNode: Option[Node]): Boolean = {
    // 左に行く時には新たに上限値が、右に行くときには新たに下限値が課せられる。
    def helper(nodeOpt: Option[Node], minValueOpt: Option[Int], maxValueOpt: Option[Int]): Boolean = {
      nodeOpt match {
        case None => true
        case Some(node) =>
          // このノードの値が範囲内にあることを確認
          minValueOpt.forall(_ < node.value) &&
            maxValueOpt.forall(_ > node.value) &&
            // 子要素に適用
            helper(node.left, minValueOpt, Some(node.value)) &&
            helper(node.right, Some(node.value), maxValueOpt)
      }
    }

    helper(rootNode, None, None)
  }




  //     5
  //    / \
  //   3   8
  //  / \ / \
  // 2  4 6  9


  // 正しいBSTの例
  val correctBST = Node(5,
    left = Some(Node(3, left = Some(Node(2)), right = Some(Node(4)))),
    right = Some(Node(7, left = Some(Node(6)), right = Some(Node(8))))
  )

  // 誤ったBSTの例（右の子が親よりも小さい）
  val incorrectBST = Node(5,
    left = Some(Node(3, left = Some(Node(2)), right = Some(Node(4)))),
    right = Some(Node(1))
  )

  println(s"Correct BST: ${isBST(Some(correctBST))}") // true
  println(s"Incorrect BST: ${isBST(Some(incorrectBST))}") // false


}
