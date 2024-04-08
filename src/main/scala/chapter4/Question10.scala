package chapter4

object Question10 extends App {

  // 部分木チェック
  // T1とT2は非常に大きい二分木で、1はT2と比べてかなり大きくなっている。
  // このとき、T2がT1の 部分木であるかどうかを判定するアルゴルムを作る。。
  // T2がT1の部分木であるということは、T1上のあるノードnについて、n以下の部分木がT2と同じであるということ。

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  // t1の木がt2の木をサブツリーとして含んでいるかどうかを判断する
  def containsTree(t1: Option[Node], t2: Option[Node]): Boolean = t2 match {
    case None => true // 空の木は全てのサブツリー
    case _ => isSubTree(t1, t2)
  }

  // t1の木がt2の木をサブツリーとして含んでいるかどうかを再帰的に確認する。
  def isSubTree(r1: Option[Node], r2: Option[Node]): Boolean = r1 match {
    case None => // t1のサブツリー切り出しをし尽くしたが一致しなかったということ
      false
    case Some(r1Node) =>
      // t1の現在のサブツリーが一致しているか。または、t1のもう一つ下（左or右）のサブツリーが一致しているか
      isSameTree(r1, r2) || isSubTree(r1Node.left, r2) || isSubTree(r1Node.right, r2)
  }

  // r1とr2の木が完全一致するかどうかを調べる
  def isSameTree(r1: Option[Node], r2: Option[Node]): Boolean = (r1, r2) match {
    case (None, None) => // 両方のノードがNoneの場合、サブツリーに残っているものがないため一致。
      true
    case (None, Some(_)) | (Some(_), None) => // 一方のノードのみがNoneの場合、不一致。
      false
    case (Some(r1Node), Some(r2Node)) => // 現在のノードおよび、配下の全てのノードが一致していることの確認
      r1Node.value == r2Node.value && isSameTree(r1Node.left, r2Node.left) && isSameTree(r1Node.right, r2Node.right)
  }


  val t1 = Node(3,
    left = Some(Node(5,
      left = Some(Node(6)),
      right = Some(Node(2,
        left = Some(Node(7)),
        right = Some(Node(4)))))),
    right = Some(Node(1,
      left = Some(Node(0)),
      right = Some(Node(8)))))

  val t2 = Node(5,
    left = Some(Node(6)),
    right = Some(Node(2,
      left = Some(Node(7)),
      right = Some(Node(4))))
  )

  val t3 = Node(1,
    left = Some(Node(0)),
    right = Some(Node(7))) // 7ではなく8であればsubTree


  println(containsTree(Some(t1), Some(t2)))
  println(containsTree(Some(t1), Some(t3)))

}
