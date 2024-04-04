package chapter3

object Question6 extends App {

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



    val shelterQueue = AnimalQueue()

    // 動物をキューに追加
    val updatedQueue1 = shelterQueue.enqueue(Dog("Rex"))
    val updatedQueue2 = updatedQueue1.enqueue(Cat("Whiskers"))
    val updatedQueue3 = updatedQueue2.enqueue(Dog("Buddy"))
    val updatedQueue4 = updatedQueue3.enqueue(Cat("Misty"))

    // キューから任意の動物を取り出す
    val (dequeueAny1, queueAfterDequeueAny1) = updatedQueue4.dequeueAny
    println(s"Dequeued Any: ${dequeueAny1.map(_.name)}") // 期待される出力: Dequeued Any: Some(Rex)

    // キューから犬を取り出す
    val (dequeueDog, queueAfterDequeueDog) = queueAfterDequeueAny1.dequeueDog
    println(s"Dequeued Dog: ${dequeueDog.map(_.name)}") // 期待される出力: Dequeued Dog: Some(Buddy)

    // キューから猫を取り出す
    val (dequeueCat, queueAfterDequeueCat) = queueAfterDequeueDog.dequeueCat
    println(s"Dequeued Cat: ${dequeueCat.map(_.name)}") // 期待される出力: Dequeued Cat: Some(Whiskers)

    // 再度、任意の動物を取り出す
    val (dequeueAny2, _) = queueAfterDequeueCat.dequeueAny
    println(s"Dequeued Any: ${dequeueAny2.map(_.name)}") // 期待される出力: Dequeued Any: Some(Misty)



}
