package chapter3

object Question2 extends App {

  // 3-2。最小値も返せるスタック
  // 最小値を管理するスタックを設ける
  case class StackWithMin(stack: List[Int] = Nil, minStack: List[Int] = Nil) {

    def min: Int = minStack.headOption.getOrElse(throw new Exception("Stack is empty"))

    def push(value: Int): StackWithMin = {
      // 新たに追加する値が現在の最小値以下の場合は、最小値管理用のスタックにも追加。
      // pop時の最小値管理スタックからの値除去制御の都合上、現在の最小値「未満」ではなく「以下」の条件としている。
      // こうしないと、例えばスタックに2つ存在する最小値の内の1つだけを取り出した場合に最小値管理スタックが空になってしまう。
      val newMinStack = if (minStack.isEmpty || value <= min) value :: minStack else minStack
      StackWithMin(value :: stack, newMinStack)
    }

    def pop: (Int, StackWithMin) = stack match {
      case stackHead :: stackTail =>
        // スタックから取り出した値が最小値管理用スタックのLast inの値と一致している場合は、その値も取り除く。
        val newMinStack = if (minStack.headOption.contains(stackHead)) minStack.tail else minStack
        (stackHead, StackWithMin(stackTail, newMinStack))
      case _ =>
        throw new Exception("Stack is empty")
    }
  }


  // スタック操作のシナリオ
  val initialStack = StackWithMin()
  val stackAfterFirstPush = initialStack.push(2) // 最小値を2回プッシュ
  val stackAfterSecondPush = stackAfterFirstPush.push(2)
  val stackAfterThirdPush = stackAfterSecondPush.push(3)
  val stackAfterFourthPush = stackAfterThirdPush.push(1) // さらに小さい値をプッシュ

  // 最小値が1であることを確認
  assert(stackAfterFourthPush.min == 1)

  // 最小値1をポップし、次の最小値が2であることを確認
  val (_, stackAfterFirstPop) = stackAfterFourthPush.pop
  assert(stackAfterFirstPop.min == 2)

  // 3をポップし、最小値が2であることを確認
  val (_, stackAfterSecondPop) = stackAfterFirstPop.pop
  assert(stackAfterSecondPop.min == 2)

  // 2をポップし、最小値がもう1つの2であることを確認
  val (_, stackAfterThirdPop) = stackAfterSecondPop.pop
  assert(stackAfterThirdPop.min == 2)

}
