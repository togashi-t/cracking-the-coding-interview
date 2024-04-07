package chapter4

import scala.annotation.tailrec

object Question6 extends App {

  // 二分探索木において与えられたノードの「次の」ノード(in - orderの走査で) を探すアルゴリズムを設計してください。
  // 各ノードは自身の親ノードへのリンクを持っていると仮定して構いません

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None, parent: Option[Node] = None)


  // 次のノードを探す
  def findNext(nodeOpt: Option[Node]): Option[Node] = nodeOpt.flatMap { node =>
    if (node.right.isDefined) { // 右部分木が存在する場合、右部分木の最も左のノードを探す
      findLeftMostChild(findLeftMostChild(node.right))
    } else { // 右部分木がない場合、親をたどって最初の右親のノードを探す
      findFirstRightParent(nodeOpt)
    }
  }


  // 特定のノードから最も左の子ノードを探す関数
  @tailrec
  def findLeftMostChild(nodeOpt: Option[Node]): Option[Node] = nodeOpt match {
    case None => None
    case Some(node) =>
      if (node.left.isEmpty) Some(node) else findLeftMostChild(node.left)
  }

  // 親ノードをたどって、最初の右親ノードを探す関数
  def findFirstRightParent(nodeOpt: Option[Node]): Option[Node] = nodeOpt.flatMap { node =>
    node.parent match {
      case None => None // 親ノードがない場合、これ以上探索できない
      case Some(parent) => // 親ノードが存在する場合
        if (parent.left.contains(node)) Some(parent) // 現在のノードが親の左子である場合
        else findFirstRightParent(parent.parent) // 現在のノードが親の右子である場合、親ノードの親へ探索を続ける
    }
  }
  

}
