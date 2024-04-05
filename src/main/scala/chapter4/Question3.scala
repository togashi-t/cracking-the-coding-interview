package chapter4

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Question3 extends App {

  abstract class TreeNode
  object EmptyNode extends TreeNode
  case class NormalNode(value: Int, left: TreeNode, right: TreeNode) extends TreeNode

  // 二分探索木が与えられたとき、同じ深さのノード同士のリストを作るアルゴリズム
  def getSameDepthList(rootNode: TreeNode): List[List[NormalNode]] = {

    // 深さ毎にNormalNodeのListを作成し、このListを積み重ねていく。
    // とある深さのNormalNodeのListから、次の深さのNormalNodeのListを生成する。
    // この生成したListが空の場合、その深さ以降にはNormalNodeが存在しない。
    @tailrec
    def loop(acc: List[List[NormalNode]]): List[List[NormalNode]] = {
      acc match {
        case Nil =>
          Nil
        case Nil :: tail =>
          tail
        case head :: _ =>
          val newHead: List[NormalNode] = head.foldLeft(List.empty[NormalNode]) { case (tmpNormalNodes, node) =>
            val leftNode = node.left
            val rightNode = node.right
            removeEmptyNode(rightNode).fold(tmpNormalNodes) { normalNode => normalNode :: tmpNormalNodes }.pipe { updatedTmpNormalNodes =>
              removeEmptyNode(leftNode).fold(updatedTmpNormalNodes) { normalNode => normalNode :: updatedTmpNormalNodes}
            }
          }

          if (newHead.isEmpty) acc else loop(newHead :: acc)
      }
    }

    loop(List(removeEmptyNode(rootNode).toList))
  }


  private def removeEmptyNode(node: TreeNode): Option[NormalNode] = {
    node match {
      case normalNode: NormalNode => Some(normalNode)
      case _ => None
    }
  }







  val testTree = NormalNode(
    5,
    NormalNode(
      3,
      NormalNode(1, NormalNode(10, EmptyNode, EmptyNode), EmptyNode),
      NormalNode(4, EmptyNode, EmptyNode)
    ),
    NormalNode(
      8,
      NormalNode(7, EmptyNode, EmptyNode),
      NormalNode(9, EmptyNode, EmptyNode)
    )
  )

  val depthLists = getSameDepthList(testTree)

  // 深い程リストの先頭に来るのでこのようにしている
  depthLists.reverse.zipWithIndex.foreach { case (list, depth) =>
    println(s"Depth $depth: ${list.map(_.value).mkString(", ")}")
  }



}
