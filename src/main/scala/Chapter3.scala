object Chapter3 extends App {

  // 3-1。3つのスタック
  // 1つの配列を使って3つのスタックを実装するにはどのようにすればよいか。
  class FixedMultiStack[T](stackSize: Int, stackCount: Int) {
    // スタックの全ての値を保持する配列
    private val values = new Array[Int](stackSize * stackCount)
    // 各スタックの現在のサイズを保持する配列
    private val sizes = new Array[Int](stackCount)

    // 各スタックの先頭要素を格納する場所の、配列全体におけるインデックスを取得
    private def getHeadIndex(stackNumber: Int) = stackNumber * stackSize


    // 指定のスタックに値を追加する場合
    def push(stackNumber: Int, value: Int): Unit = {
      // 当該スタックに現在格納されている個数
      val size = sizes(stackNumber)
      if (size == stackSize) {
        throw new Exception("Stack is already full")
      } else {
        val newSize = size + 1
        values(getHeadIndex(stackNumber) + (newSize - 1)) = value // -1しているのは先頭に1つめが格納されるから
        sizes(stackNumber) = newSize
      }
    }

    //　指定のスタックから最も最近追加した要素を取り出す
    def pop(stackNumber: Int): Unit = {
      // 当該スタックに現在格納されている個数
      val size = sizes(stackNumber)
      if (size == 0) {
        throw new Exception("Stack is already empty")
      } else {
        values(getHeadIndex(stackNumber) + (size - 1)) = 0 // 空となった場所には0を入れる
        val newSize = size - 1
        sizes(stackNumber) = newSize
      }
    }

    // 指定されたスタックの最も最近追加した要素を返す
    def peek(stackNumber: Int): Int = {
      // スタックが空かどうかチェック
      val size = sizes(stackNumber)
      if (size == 0) {
        throw new Exception("Stack is empty.")
      }
      // スタックのトップの値を返す
      values(getHeadIndex(stackNumber) + (size - 1))
    }

  }



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


  // 3-3。複数のスタック。
  // いくつかのスタックを持ち、スタックのデータが一杯になったらスタックを新たに作成する。
  // pushやpopは普通の1つのスタックのように振舞う。
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







}
