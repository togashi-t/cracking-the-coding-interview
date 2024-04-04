package chapter3

import scala.annotation.tailrec

object Question4 extends App {

  // 3-4。スタックでキュー。2つのスタックを用いたキューを実装する。
  // pop用とpush用のスタックを用意する。
  // push先は両Stackが空の場合のみpopStack、それ以外はpushStack。pushStackの一番上を最も古い要素とするため。
  // 上記のようにするのは、pushStackを反転されるコストを少しでも減らしたいため。
  case class MyQueue[T](popStack: List[T] = Nil, pushStack: List[T] = Nil) {

    @tailrec
    final def pop: (Option[T], MyQueue[T]) = {
      this match {
        case MyQueue(Nil, Nil) =>
          (None, MyQueue())
        case MyQueue(Nil, push) =>
          MyQueue(push.reverse, Nil).pop
        case MyQueue(popHead :: popTail, _) =>
          (Some(popHead), this.copy(popStack = popTail))
      }
    }

    def push(value: T): MyQueue[T] = if (popStack.isEmpty && pushStack.isEmpty)
      MyQueue(List(value), pushStack)
    else {
      MyQueue(popStack, value :: pushStack)
    }

  }




    // 初期状態のキューを作成
    var queue = MyQueue[Int]()

    // キューに要素を追加
    queue = queue.push(1)
    println("After pushing 1: " + queue)

    // キューにさらに要素を追加
    queue = queue.push(2)
    println("After pushing 2: " + queue)

    // キューから要素を一つ取り出し
    val (popResult1, queueAfterPop1) = queue.pop
    println(s"Popped: $popResult1, Queue after pop: " + queueAfterPop1)

    // 再度要素を追加
    queue = queueAfterPop1.push(3)
    println("After pushing 3: " + queue)

    // 再度要素を取り出す
    val (popResult2, queueAfterPop2) = queue.pop
    println(s"Popped: $popResult2, Queue after pop: " + queueAfterPop2)

    // キューが空になった状態で、さらに要素を追加
    queue = queueAfterPop2.push(4)
    println("After pushing 4: " + queue)

    // キューから要素を取り出す
    val (popResult3, queueAfterPop3) = queue.pop
    println(s"Popped: $popResult3, Queue after pop: " + queueAfterPop3)

    // キューが空の状態で、さらにpopを試みる
    val (popResult4, queueAfterPop4) = queueAfterPop3.pop
    println(s"Popped: $popResult4, Queue after pop: " + queueAfterPop4)

    // キューが空になった後に、再度要素を追加し、取り出す
    queue = queueAfterPop4.push(5).push(6)
    println("After pushing 5 and 6: " + queue)

    // 一度に複数の要素を取り出す
    val (popResult5, queueAfterPop5) = queue.pop
    val (popResult6, finalQueue) = queueAfterPop5.pop
    println(s"Popped: $popResult5, Queue after first pop: " + queueAfterPop5)
    println(s"Popped: $popResult6, Final queue: " + finalQueue)


}
