object PerformanceCheck extends App {

  private val runtime = Runtime.getRuntime

  // メモリ使用量を計算するヘルパー関数
  private def usedMemory(): Long = runtime.totalMemory() - runtime.freeMemory()

  // 引数で与えられた処理を実行し、実行時間とメモリ使用量を測定する関数
  private def check[T](process: => T): Unit = {
    runtime.gc() // ガベージコレクタを強制実行
    Thread.sleep(100) // GCが完了するのを待つ

    val startTime = System.nanoTime()
    val startMemory = usedMemory()

    // 実行したい処理
    process

    val endMemory = usedMemory()
    val endTime = System.nanoTime()

    // 測定結果の出力
    println(s"Execution time: ${(endTime - startTime) / 1e9} seconds")
    println(s"Memory usage: ${(endMemory - startMemory) / 1024} KB")
  }


  // todo 測定対象の処理を渡す

  check {

  }

}
