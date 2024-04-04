package chapter3

import scala.collection.mutable

object Question3 extends App {

  // 3-3。複数のスタック。 いくつかのスタックを持ち、スタックのデータが一杯になったらスタックを新たに作成する。
  // pushやpopは普通の1つのスタックのように振舞う。

  // 3-3-1。通常版
  case class SetOfStacks[T](stacks: List[List[T]] = Nil, currentHeadStockSize: Int = 0) {
    // 1つのスタックに格納できる最大要素数を定義する。
    private val stackLimitSize = 3

    def push(value: T): SetOfStacks[T] = stacks match {
      case Nil =>
        SetOfStacks(List(List(value)), 1)
      case headStack :: tailStacks =>
        if (currentHeadStockSize == stackLimitSize) { // 先頭スタックが既に満杯の場合
          val newStacks = List(value) :: stacks
          SetOfStacks(newStacks, 1)
        } else { // 先頭スタックにまだ空き容量がある場合
          val newStacks = (value :: headStack) :: tailStacks
          SetOfStacks(newStacks, currentHeadStockSize + 1)
        }
    }

    // スタック自体が空のとき以外で先頭スタックが空になることは無いようにする。
    // 空のスタックを残すと、捜査対象とすべき先頭スタックがどれなのかわからなくなってしまうので。
    def pop: (T, SetOfStacks[T]) = stacks match {
      case Nil =>
        throw new Exception("Stack is empty")
      case headStack :: tailStacks =>
        headStack match {
          case Nil => // 先頭スタックが空の場合
            throw new Exception("Unexpected error")
          case headStackHead :: Nil => // 先頭スタックに1つしか要素が存在しない場合
            // 他のスタックが存在しない場合は、pop後のcurrentHeadStockSizeを0とする
            (headStackHead, SetOfStacks(tailStacks, if (tailStacks.isEmpty) 0 else stackLimitSize))
          case headStackHead :: headStackTail => // 先頭スタックに2つ以上の要素が存在する場合
            (headStackHead, SetOfStacks(headStackTail :: tailStacks, currentHeadStockSize - 1))
        }
    }

  }


  // 3-3-2。発展版。指定のスタックからpopする関数がある
  class setOfStacksB[T] {
    // 1つのスタックに格納できる最大要素数を定義する。
    private val stackLimitSize = 3

    // 実際に値を格納するコレクション
    private val stacks = mutable.ArrayDeque[mutable.ArrayDeque[T]]()

    def push(value: T): Unit = {
      (if (stacks.isEmpty || stacks.last.size == stackLimitSize) {
        stacks.append(mutable.ArrayDeque[T]())
      } else {
        stacks
      }).last.append(value)
    }

    def pop(): Option[T] = {
      if (stacks.isEmpty) {
        None
      } else {
        val valueOpt = stacks.last.removeLastOption()
        // もし上記の値取出によりそのスタックが空になった場合はそのスタックを削除
        if (stacks.last.isEmpty) stacks.removeLast()
        valueOpt
      }
    }

    def popAt(index: Int): Option[T] = {
      if (index < 0 || index >= stacks.length) {
        None
      } else {
        val stack = stacks(index)
        if (stack.isEmpty) {
          None
        } else {
          val valueOpt = stack.removeLastOption()
          // 要素が空いた場所を埋める処理


          valueOpt
        }
      }
    }

    // 要素が空いた場所を他の要素を詰めて埋める処理
    private def shiftLeft(index: Int): Unit = {
      // 指定されたインデックスのスタックから次のスタックへ要素をシフト
      for (i <- index until stacks.length - 1) {
        val nextStack = stacks(i + 1)
        if (nextStack.nonEmpty) {
          val movedValue = nextStack.removeHead()
          stacks(i).append(movedValue)
        }
      }
      // 最後のスタックが空になっていたら削除
      if (stacks.last.isEmpty) {
        stacks.removeLast()
      }
    }

  }

}
