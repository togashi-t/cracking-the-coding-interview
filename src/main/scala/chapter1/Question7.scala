package chapter1

object Question7 extends App {

  // 行列を右に90度回転
  // 1-7-1。要素を1回で移動先へ移動する方法。時間計算量の点で優れているが、コードが難しい。
  def rotateA(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    // 1辺の長さ
    val length = matrix.length
    // 辺の長さが1以下の場合は回転する余地がないので即終了
    if (length <= 1) {
      matrix
    } else {
      // 外側の辺(=層)から順に他の辺の点を置換していく
      for (layerIndex <- 0 until length / 2) {
        val firstIndex = layerIndex
        val lastIndex = length - 1 - layerIndex

        for (i <- firstIndex until lastIndex) {
          // 現在の要素からレイヤーの最初の位置までの距離
          val offset = i - firstIndex

          // 上辺の要素の値をとっておく
          val tmp = matrix(firstIndex)(i)

          // 左辺から上辺への移動
          matrix(firstIndex)(i) = matrix(lastIndex - offset)(firstIndex)

          // 下辺から左辺への移動
          matrix(lastIndex - offset)(firstIndex) = matrix(lastIndex)(lastIndex - offset)

          // 右辺から下辺への移動
          matrix(lastIndex)(lastIndex - offset) = matrix(i)(lastIndex)

          // 上側(保存した値) -> 右側
          matrix(i)(lastIndex) = tmp
        }
      }

      matrix
    }
  }


  // 1-7-2。行列を入れ替えた後、列を入れ替える方法。時間計算量の点で先の方法より劣るが、コードの理解は容易。
  def rotateB(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    // 1辺の長さ
    val length = matrix.length
    // 辺の長さが1以下の場合は回転する余地がないので即終了
    if (length <= 1) {
      matrix
    } else {
      // 行列の入替
      for {
        rowIndex <- 0 until length
        colIndex <- rowIndex + 1 until length
      } {
        val tmp = matrix(rowIndex)(colIndex)
        matrix(rowIndex)(colIndex) = matrix(colIndex)(rowIndex)
        matrix(colIndex)(rowIndex) = tmp
      }

      // 列の入替
      for {
        rowIndex <- 0 until length
        colIndex <- 0 until length / 2
      } {
        val tmp = matrix(rowIndex)(colIndex)
        matrix(rowIndex)(colIndex) = matrix(rowIndex)(length - 1 - colIndex)
        matrix(rowIndex)(length - 1 - colIndex) = tmp
      }

      matrix
    }

  }


  val testMatrices = Seq(
    Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)),
    Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12), Array(13, 14, 15, 16))
  )

  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    matrix.foreach { row =>
      println(row.mkString(" "))
    }
    println() // 行列間の空行
  }

  testMatrices.zipWithIndex.foreach { case (matrix, index) =>
    println(s"Original Matrix $index:")
    printMatrix(matrix)

    val rotatedMatrix = rotateB(matrix)
    println(s"Rotated Matrix $index:")
    printMatrix(rotatedMatrix)
  }

}
