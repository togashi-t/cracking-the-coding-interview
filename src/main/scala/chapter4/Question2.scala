package chapter4

import scala.util.chaining.scalaUtilChainingOps

object Question2 extends App {

  abstract class TreeNode
  object EmptyNode extends TreeNode
  case class NormalNode(value: Int, left: TreeNode, right: TreeNode) extends TreeNode


  // 昇順にソートされた全ての要素が異なる配列を与えられたとき、高さが最小になる二分探索木を作るアルゴリズムを作成する。
  def genLowestTree(array: Array[Int]): TreeNode = {

    // 配列の特定範囲を再帰的に処理して部分木を作成する
    def loop(headIndex: Int, lastIndex: Int): TreeNode = {
      if (headIndex > lastIndex) { // headIndex == lastIndexとなるのは、子ノードが1つもないノードを作成する場合。
        EmptyNode
      } else {
        // rootになる要素のindex
        val middleIndex = (lastIndex + headIndex) / 2
        NormalNode(
          value = array(middleIndex),
          left = loop(headIndex, middleIndex - 1), // 左部分木を作成
          right = loop(middleIndex + 1, lastIndex) // 右部分木を作成
        )
      }
    }

    loop(0, array.length - 1)
  }



  val testArray = Array(1, 2, 3, 4, 5, 6, 7)

  val tree = genLowestTree(testArray)

  def printTree(node: TreeNode, prefix: String = ""): Unit = node match {
    case EmptyNode => println(prefix + "X") // 空のノードを "X" として表示
    case NormalNode(value, left, right) =>
      println(prefix + value) // 現在のノードの値を表示
      printTree(left, prefix + "L:") // 左の子ノードを再帰的に表示
      printTree(right, prefix + "R:") // 右の子ノードを再帰的に表示
  }

  printTree(tree)


}
