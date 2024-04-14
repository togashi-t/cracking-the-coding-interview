package chapter5

object Question8 extends App {

  // 直線を描く:モノクロのスクリーンが1次元のバイト型配列として保持されています。
  // 1バイトには連続した8ピクセルを保持することができます。
  // スクリーンの幅は8の倍数で、バイトの途中で切れるような形にはなっていないことにします。
  // 当然ですが、スクリーンの高さは配列のサイズとスクリーンの幅から計算することができます。
  // このとき、(×1,y)から(×2,y)まで水平な直線を描く関数を実装してください。
  // メソッドのシグネチャは以下のようにします。
  // drawLine(byte[] screen, int width, int x1, int x2, int y)


  def drawLine(screen: Array[Byte], width: Int, x1: Int, x2: Int, y: Int): Array[Byte] = {
    // 各行の開始バイトを計算し、バイト単位での開始位置と終了位置を特定
    val startOffset = y * (width / 8) + x1 / 8
    val endOffset = y * (width / 8) + x2 / 8

    // 最終結果として使用する配列
    val newScreen = screen.clone()

    // 1バイト全てを変更する処理
    // 開始オフセットと終了オフセットの間の全バイトを最大値（すべてのビットが1）に設定
    for (i <- startOffset + 1 until endOffset) {
      newScreen(i) = 0xFF.toByte // 0xFFは全ビットが1のバイト
    }

    // 1バイトの内の一部ビットを変更する処理
    // 開始バイトと終了バイトのためのマスク
    // x1%8はバイト内での開始ビット位置、x2%8はバイト内での終了ビット位置
    val startMask = (0xFF >>> (x1 % 8)).toByte // 開始ビットからバイトの終わりまでを1にするマスク
    val endMask = (0xFF << (7 - x2 % 8)).toByte // バイトの始まりから終了ビットまでを1にするマスク

    if (startOffset == endOffset) {
      // 開始オフセットと終了オフセットが同じ場合、開始マスクと終了マスクの論理積を適用
      newScreen(startOffset) = (newScreen(startOffset) | (startMask & endMask)).toByte
    } else {
      // 開始オフセットと終了オフセットが異なる場合、それぞれ独立してマスクを適用
      newScreen(startOffset) = (newScreen(startOffset) | startMask).toByte
      newScreen(endOffset) = (newScreen(endOffset) | endMask).toByte
    }

    newScreen
  }

  
}
