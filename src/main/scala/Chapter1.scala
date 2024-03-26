import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Chapter1 extends App {

  // 1-1-1
  // 新たなデータ構造を使用し、そして文字コードがASCIIである場合。
  def isNoDuplicationStringA(str: String): Boolean = {
    val numberOfTypesCharacters = 128

    if (str.length > numberOfTypesCharacters) { // 文字種の数を超過している場合。必ず重複が存在する。
      false
    } else {
      // 各文字の存在数を記録するための配列
      val charSet = Array.fill(numberOfTypesCharacters)(0)

      @tailrec
      def loop(str: String, currentStrIndex: Int, maxStrIndex: Int): Boolean = {
        if (currentStrIndex > maxStrIndex) { // 全ての文字を走査し終えた場合
          true
        } else {
          val char = str(currentStrIndex)
          val asciiValue = char.toInt
          if (charSet(asciiValue) > 0) { // この文字の存在数記録を確認
            false
          } else {
            charSet(asciiValue) = 1
            loop(str, currentStrIndex + 1, maxStrIndex)
          }
        }
      }

      loop(str, 0, str.length - 1)
    }
  }


  // 1-1-2
  // 新たなデータ構造を使用しない場合
  def isNoDuplicationStringB(str: String): Boolean = {
    // 文字列の長さが1以下の場合は重複がないので
    if (str.length <= 1) {
      true
    } else {
      // 文字列を整列させる
      val sortedStr = str.sorted

      // 隣り合う文字列で同一のものがないか確認する。文字列の先頭から順番に2つの文字を取り出して確認。同一のものがあった時点でfalseとして終了。
      @tailrec
      def loop(sortedStr: String, currentHeadIndex: Int, maxHeadIndex: Int): Boolean = {
        if (currentHeadIndex > maxHeadIndex) { // 走査完了の場合
          true
        } else {
          if (sortedStr(currentHeadIndex) == sortedStr(currentHeadIndex + 1)) {
            false
          } else {
            loop(sortedStr, currentHeadIndex + 1, maxHeadIndex)
          }
        }
      }

      loop(sortedStr, 0, sortedStr.length - 2)
    }
  }


  // 1-2-0。単純に真逆に並んでいることを確認する（書籍の問題の意味はこれではなかった）
  def isReverse(strA: String, strB: String): Boolean = {
    // そもそも長さが異なる場合はその時点でfalse
    val lengthOfA = strA.length
    val lengthOfB = strB.length

    if (lengthOfA != lengthOfB) {
      false
    } else {
      // 一方の要素は先頭から、もう一方の要素は末尾から要素を取得し、同一であるか確認する。
      @tailrec
      def loop(strA: String, strB: String, currentIndex: Int, lastIndex: Int): Boolean = {
        if (currentIndex > lastIndex) {
          true
        } else {
          if (strA(currentIndex) != strB(lastIndex - currentIndex)) {
            false
          } else {
            loop(strA, strB, currentIndex + 1, lastIndex)
          }
        }
      }

      loop(strA, strB, 0, lengthOfA - 1)
    }
  }


  // 1-2。2つの文字列が同じ文字で構成されているか。
  def isSameCharacters(strA: String, strB: String): Boolean = {
    if (strA.length != strB.length) {
      false
    } else {
      // 2つの文字を構成している文字の数をまとめる
      val strACountMap = strA.groupBy(identity).view.mapValues(_.length).toMap
      val strBCountMap = strB.groupBy(identity).view.mapValues(_.length).toMap

      strACountMap == strBCountMap
    }
  }


  // 1-3。空白文字を指定文字で置換。ただし、strについては指定された長さ以降は切り捨てる。
  def replaceAllSpace(str: String, trueLength: Int): String = {
    str.take(trueLength).replaceAll(" ", "%20")
  }


  // 文字列を並び替えることによって回文作成が可能かを返す。
  // 1-4-1。数を数える方法。
  def isRearrangeableToPalindromeA(str: String): Boolean = {
    val charCountMap = str
      .replaceAll(" ", "") // スペースは考慮の対象外なので除外
      .toLowerCase // 大文字小文字の区別はしないので全て小文字で処理する
      .groupBy(identity).view.mapValues(_.length).toMap // 文字毎の出現回数を集計
    // 使用回数が奇数回の文字が1個以下の場合は回文作成が可能
    charCountMap.values.count(_ % 2 == 1) <= 1
  }

  // 1-4-2。文字種類毎にtoggleする方法
  def isRearrangeableToPalindromeB(str: String): Boolean = {
    val oddNumberCharSet = scala.collection.mutable.Set.empty[Char]
    val lowerCaseStr = str
      .replaceAll(" ", "") // スペースは考慮の対象外なので除外
      .toLowerCase // 大文字小文字の区別はしないので全て小文字で処理する
    // 文字の出現ごとにSetの中の当該文字をtoggleする
    for (char <- lowerCaseStr) {
      if (oddNumberCharSet.contains(char)) oddNumberCharSet -= char else oddNumberCharSet += char
    }

    // 使用回数が奇数回の文字が1個以下の場合は回文作成が可能
    oddNumberCharSet.size <= 1
  }


  // 1-5。一発変換可否。2つの文字列が与えられたとき、一方の文字列に対して1操作（文字の挿入or削除or置換）（または操作なし）でもう一方の文字列に変換できるか。
  def isConvertable(strA: String, strB: String): Boolean = {
    val strALength = strA.length
    val strBLength = strB.length

    if (scala.math.abs(strALength - strBLength) >= 2) { // 文字列の長さの差が2以上の場合は不可
      false
    } else if (strALength == strBLength) { // 文字列の長さが同一の場合は、相違している文字数が1以下の場合は可math.abs
      @tailrec
      def loop(strA: String, strB: String, currentIndex: Int, maxIndex: Int, diffCount: Int): Boolean = {
        if (currentIndex > maxIndex) { // 全ての捜査が完了の場合
          diffCount <= 1
        } else if (diffCount > 1) { // 既に違う文字数が1より大きい場合
          false
        } else {
          loop(strA, strB, currentIndex + 1, maxIndex, if (strA(currentIndex) == strB(currentIndex)) diffCount else diffCount + 1)
        }
      }

      loop(strA, strB, 0, strALength - 1, 0)
    } else { // 文字列の長さが1だけ異なる場合
      // 短い方の文字列に1つの文字を追加して長い文字列となるかを確認する。
      // 具体的な方法は、
      // (1)両文字列のindex対象文字が同じであるか比較。初めのindexは両方共0
      // (2)もし同じ場合は両indexに+1。もし異なる場合は長い文字のみindexを+1する。
      // (1)(2)を繰り返すが、indexが最後まで到達したらtrue、その前にindexの差が2となったらfalse。
      val (shorterStr, longerStr) = if (strALength < strBLength) (strA, strB) else (strB, strA)
      val (shorterStrMaxIndex, longerStrMaxIndex) = (scala.math.min(strALength, strBLength) - 1, scala.math.max(strALength, strBLength) - 1)

      @tailrec
      def loop(shorterStrCurrentIndex: Int, longerStrCurrentIndex: Int): Boolean = {
        if (shorterStrCurrentIndex <= shorterStrMaxIndex && longerStrCurrentIndex <= longerStrMaxIndex) { // まだ最後の文字に到達していない場合。
          if (shorterStr(shorterStrCurrentIndex) == longerStr(longerStrCurrentIndex)) { // 文字が一致した場合
            loop(shorterStrCurrentIndex + 1, longerStrCurrentIndex + 1) // 参照するindexを両方共進める
          } else if (shorterStrCurrentIndex != longerStrCurrentIndex) { // 2回目の文字不一致
            false
          } else { // 1回目の不一致。長い方の文字のindexを1つずらす。
            loop(shorterStrCurrentIndex, longerStrCurrentIndex + 1)
          }
        } else {
          true
        }
      }

      loop(0, 0)
    }
  }


  // 1-6。文字列圧縮。元の文字列よりも短くならなかった場合は元の文字列を返す
  def compressStr(str: String): String = {
    val strLength = str.length
    // 圧縮結果を格納しているコレクション。圧縮結果が元のstrより短い場合のみ圧縮結果に意味があるため、その長さを設定。
    val compressedStrBuilder = new StringBuilder(strLength - 1)

    val maxIndex = strLength - 1

    @tailrec
    def loop(currentIndex: Int, currentCompressedLength: Int, consecutiveChar: Char, consecutiveCount: Int): String = {
      if (currentCompressedLength >= strLength) { // 元の文字列より短くない場合
        str
      } else if (currentIndex > maxIndex) { // 末尾の文字まで処理が完了した場合
        compressedStrBuilder.append(s"${consecutiveChar}${consecutiveCount}") // 連続した数を記録
        compressedStrBuilder.toString
      } else {
        val currentChar = str(currentIndex)
        if (currentChar == consecutiveChar) { // 文字の連続が継続している場合
          loop(currentIndex + 1, currentCompressedLength, consecutiveChar, consecutiveCount + 1)
        } else { // 文字の連続が経たれた場合
          compressedStrBuilder.append(s"${consecutiveChar}${consecutiveCount}") // 連続した数を記録
          loop(currentIndex + 1, currentCompressedLength + (1 + countDigit(consecutiveCount)) /** 文字の長さ＋数の長さ */, currentChar, 1)
        }
      }
    }

    loop(1, 0, str(0), 1)
  }


  // 整数の桁数を求める。n.toString.lengthだとnの数が大きい場合にパフォーマンスが悪化するため。
  private def countDigit(n: Int): Int = {
    if (n == 0) 1
    else (math.log10(n.toDouble).pipe(math.floor) + 1).toInt // toDoubleを用いているのは、log10の引数の型はDoubleとする必要があるため。
  }



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


  // 1-8
  // 行列の初期状態でとある要素がゼロの場合、その要素と同一の行および列の全ての要素をゼロにする。
  def spreadZero(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    // ゼロが1つでもある行または列は全てゼロになるので、当該行および列のindexを記録する。
    val zeroRowIndexes = scala.collection.mutable.Set.empty[Int]
    val zeroColumnIndexes = scala.collection.mutable.Set.empty[Int]

    val rowCount = matrix.length
    val columnCount = matrix(0).length

    for {
      rowIndex <- 0 until rowCount
      columnIndex <- 0 until columnCount
      // 行および列がゼロであることが既に決定している場合は要素の値確認をスキップする
      if !(zeroRowIndexes.contains(rowIndex) && zeroColumnIndexes.contains(columnIndex))
    } {
      if (matrix(rowIndex)(columnIndex) == 0) {
        zeroRowIndexes.add(rowIndex)
        zeroColumnIndexes.add(columnIndex)
      }
    }

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



  // 1-9。片方の文字列がもう片方の文字列を回転させたものであるかを判定する。
  def isRotated(str1: String, str2: String): Boolean = {
    // 以下の関数を1回だけ使用することが設問の要件となっている
    def isSubstring(str: String, subStr: String) = str.contains(subStr)

    if (str1.length != str2.length) { // そもそも長さが異なる場合はその時点でfalse
      false
    } else {
      isSubstring(str = s"${str1}${str1}", subStr = str2)
    }
  }

  println(isRotated("waterbottle", "erbottlewat"))






}
