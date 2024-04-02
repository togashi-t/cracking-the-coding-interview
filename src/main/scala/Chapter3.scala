import scala.annotation.tailrec
import scala.collection.mutable

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


  // 3-5。スタックのソート。
  // 最も小さい項目がトップに来るスタックを並び替えるプログラムを書く。別のスタックを1つ用意してもOK。
  // 実際的にはリストの並び替えと同義なので、リストの並び替えを実装する。
  // 別のリストに降順での並び替えが完了がゴール。

  // 3-5-1
  def sortListA(list: List[Int]): List[Int] = {
    /**
     * 並び替えを行う処理。処理概要は以下。
     * ・mainListからsubListに値を1つずつ移していく。
     * ・この時降順になるようにしたいので、subList先頭値未満の場合は以下を行う。
     * 　・pendingOptに値を保持
     * 　・subListからmainListに値を1つずつ移していく。
     * 　・上記の際、subList先頭値がpendingの値以下となったら、pendingの値をsubListに追加する。
     * @param mainList
     * @param subList
     * @param pendingOpt
     */

    @tailrec
    def sort(mainList: List[Int], subList: List[Int], pendingOpt: Option[Int]): List[Int] = {
      if (mainList.isEmpty && subList.isEmpty) { // そもそも並び替え対象のリストが空の場合はここに来る
        Nil
      } else {
        pendingOpt match {
          case None => // mainListからsubListに値を移す
            mainList match {
              case mainHead :: mainTail =>
                subList match {
                  case subHead :: subTail if subHead > mainHead => // 移すとsubListに降順にならない場合
                    sort(subHead :: mainTail, subTail, Some(mainHead))
                  case _ =>
                    sort(mainTail, mainHead :: subList, None)
                }
              case Nil => // 全てsubListに移し終えた場合
                subList
            }
          case Some(pending) => // pendingとの大小関係を確認の上で、subListからmainListに値を移す
            subList match {
              case subHead :: subTail =>
                if (pending >= subHead) { // pending終了の場合
                  sort(mainList, pending :: subList, None)
                } else { // pending継続の場合
                  sort(subHead :: mainList, subTail, Some(pending))
                }
              case Nil =>
                sort(mainList, List(pending), None)
            }
        }
      }
    }

    sort(list, Nil, None)
  }



  // 3-5-2。再帰を使用。末尾再帰にできない部分があるので、データ量が多すぎるとスタックオーバーフローが発生する
  def sortListB(list: List[Int]): List[Int] = {

    // elementをsortedList(降順)に追加する。降順を維持するかたちで。返すのは、elementを追加した後の降順のリスト
    def insertIntoSortedList(element: Int, sortedList: List[Int]): List[Int] = {
      sortedList match {
        case head :: tail =>
          if (element >= head) element :: sortedList else head :: insertIntoSortedList(element, tail)
        case Nil =>
          List(element)
      }
    }

    @tailrec
    def sort(mainList: List[Int], sortedList: List[Int]): List[Int] = {
      mainList match {
        case head :: tail =>
          val updatedSortedList = insertIntoSortedList(head, sortedList)
          sort(tail, updatedSortedList)
        case Nil =>
          sortedList
      }
    }

    sort(list, Nil)
  }



  // 3-6。動物保護施設
  // イヌとネコしか入ることのできない動物保護施設があります。この施設は「先入れ先出し」の操作を厳格に行います。
  // 施設からは一番長い時間入っている動物を外に出すか、イヌとネコの好きなほう(で一番長い時間入っているもの)を外に出すことができます。
  // どの動物でも好きように連れ出せるわけではありません。このような仕組みを扱うデータ構造を作ってください。
  // さらにenqueue、dequeueAny、dequeueDog、dequeueCatの操作を実装してください。

  abstract class Animal(val name: String)
  case class Dog(override val name: String) extends Animal(name)
  case class Cat(override val name: String) extends Animal(name)
  case class AnimalWithSerial[T <: Animal](animal: T, serialNumber: Int)

  case class AnimalQueue(
    dogs: collection.immutable.Queue[AnimalWithSerial[Dog]] = collection.immutable.Queue.empty,
    cats: collection.immutable.Queue[AnimalWithSerial[Cat]] = collection.immutable.Queue.empty,
    latestSerialNumber: Int = 0
  ) {

    // 動物（DogまたはCat ）が渡された時その種類に応じて適切なキューに追加
    // 各動物には一意のシリアル番号を割当
    // latestSerialNumberは自動的にインクリメント
    def enqueue(animal: Animal): AnimalQueue = animal match {
      case dog: Dog =>
        this.copy(dogs = dogs.enqueue(AnimalWithSerial(dog, latestSerialNumber + 1)), latestSerialNumber = latestSerialNumber + 1)
      case cat: Cat =>
        this.copy(cats = cats.enqueue(AnimalWithSerial(cat, latestSerialNumber + 1)), latestSerialNumber = latestSerialNumber + 1)
    }

    // dogsとcatsキューの先頭を比較し、最も早くキューに追加された動物（最小のシリアル番号を持つ動物）を取り出す。
    // 取り出された動物について更新されたキューを含む新しいAnimalQueueインスタンスを返す
    def dequeueAny: (Option[Animal], AnimalQueue) = (dogs.headOption, cats.headOption) match {
      case (Some(dogWithSerial), Some(catWithSerial)) =>
        if (dogWithSerial.serialNumber < catWithSerial.serialNumber) dequeueDog else dequeueCat
      case (Some(_), None) =>
        dequeueDog
      case (None, Some(_)) =>
        dequeueCat
      case (None, None) =>
        (None, this)
    }

    def dequeueDog: (Option[Dog], AnimalQueue) = dogs.dequeueOption match {
      case Some((dogWithSerial, newQueue)) =>
        (Some(dogWithSerial.animal), this.copy(dogs = newQueue))
      case None =>
        (None, this)
    }

    def dequeueCat: (Option[Cat], AnimalQueue) = cats.dequeueOption match {
      case Some((catWithSerial, newQueue)) =>
        (Some(catWithSerial.animal), this.copy(cats = newQueue))
      case None =>
        (None, this)
    }

  }


}
