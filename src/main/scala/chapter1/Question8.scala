package chapter1

import scala.annotation.tailrec

object Question8 extends App {

  // 1-8
  // 行列の初期状態でとある要素がゼロの場合、その要素と同一の行および列の全ての要素をゼロにする。
  def spreadZero(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val rowCount = matrix.length
    val columnCount = matrix(0).length

    // ゼロが1つでもある行または列は全てゼロになるので、当該行および列のindexを洗い出す。
    // 1行ずつ順番に要素を確認していく。
    @tailrec
    def findZeroPosition(rowIndex: Int, zeroRowIndexes: Set[Int], zeroColumnIndexes: Set[Int]): (Set[Int], Set[Int]) = {
      if (rowIndex == rowCount) { // 全ての走査を完了したとき
        (zeroRowIndexes, zeroColumnIndexes)
      } else {
        val (updatedZeroRowIndexes, updatedZeroColumnIndexes) = (0 until columnCount).foldLeft((zeroRowIndexes, zeroColumnIndexes)) { case ((tmpZeroRowIndexes, tmpZeroColumnIndexes), columnIndex) =>
          if (matrix(rowIndex)(columnIndex) == 0) (tmpZeroRowIndexes + rowIndex, tmpZeroColumnIndexes + columnIndex) else (tmpZeroRowIndexes, tmpZeroColumnIndexes)
        }

        findZeroPosition(rowIndex + 1, updatedZeroRowIndexes, updatedZeroColumnIndexes)
      }
    }

    val (zeroRowIndexes, zeroColumnIndexes) = findZeroPosition(0, Set.empty[Int], Set.empty[Int])

    // ゼロにすべき行および列の要素をゼロに更新
    for {
      rowIndex <- 0 until rowCount
      columnIndex <- 0 until columnCount
      if (zeroRowIndexes.contains(rowIndex) || zeroColumnIndexes.contains(columnIndex))
    } {
      matrix(rowIndex)(columnIndex) = 0
    }

    matrix
  }


  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    matrix.foreach { row =>
      println(row.mkString(" "))
    }
    println() // 行列間の空行
  }

  val testMatrices = Seq(
    Array(Array(1, 2, 3), Array(4, 0, 6), Array(7, 8, 9)),
    Array(Array(0, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12), Array(13, 14, 15, 16)),
    Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 0, 12), Array(13, 14, 15, 16)),
    Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12), Array(0, 14, 15, 16))
  )

  testMatrices.zipWithIndex.foreach { case (matrix, index) =>
    println(s"Original Matrix $index:")
    printMatrix(matrix)

    val modifiedMatrix = spreadZero(matrix)
    println(s"Modified Matrix $index:")
    printMatrix(modifiedMatrix)
  }


}
