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


  val stack = new FixedMultiStack(2, 3)
//  hoge.pop(0)
  stack.push(1, 10)
  stack.push(1, 11)
  stack.pop(1)
  stack.push(1, 20)
  println(stack.peek(1))


}
