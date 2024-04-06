package chapter4

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Question4 extends App {

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  // 二分木が平衡かどうかを調べる関数を実装。
  // 平衡木とは全てのノードが持つ2つの部分木について、高さの差が1以下であるような木。

  // 同一の深さ毎にノードを調べていく。最も浅い底の数値を保持しながら。
  // 底=そのノードに子が存在しない状態
  def isEquilibrium(rootNode: Option[Node]): Boolean = {
    @tailrec
    def loop(currentNodes: List[Node], currentDepth: Int, shallowestDepthOpt: Option[Int]): Boolean = {

      if (currentNodes.isEmpty) { // もう走査するものがないとき
        true
      } else {
        val (nextNodes, isBottomPresent) = currentNodes.foldLeft((List.empty[Node], false)) { case ((tmpNextNodes, tmpIsBottomPresent), node) =>
          val newTmpNextNodes = node.right.toList ::: node.left.toList ::: tmpNextNodes
          val newIsBottomPresent = tmpIsBottomPresent || (node.right.isEmpty || node.left.isEmpty)
          (newTmpNextNodes, newIsBottomPresent)
        }

        shallowestDepthOpt match {
          case Some(shallowestDepth) => // 比較する深さがある場合
            if (isBottomPresent && (currentDepth - shallowestDepth > 1)) { // このたび底があり、深さの差が1より大きい場合
              false
            } else {
             loop(nextNodes, currentDepth + 1, shallowestDepthOpt)
            }
          case _ =>
            loop(nextNodes, currentDepth + 1, if (isBottomPresent) Some(currentDepth) else None)
        }
      }
    }

    loop(rootNode.toList, 1, None)
  }






  // 平衡である二分木
  val balancedTree = Node(1,
    left = Some(Node(2, left = Some(Node(4)), right = Some(Node(5)))),
    right = Some(Node(3, left = Some(Node(6)), right = Some(Node(7))))
  )

  // 平衡でない二分木
  val unbalancedTree = Node(1,
    left = Some(Node(2, left = Some(Node(3, left = Some(Node(4))))))
  )

  // 平衡な木のチェック
  println(s"Balanced Tree: ${isEquilibrium(Some(balancedTree))}") // true

  // 平衡でない木のチェック
  println(s"Unbalanced Tree: ${isEquilibrium(Some(unbalancedTree))}") // false


}
