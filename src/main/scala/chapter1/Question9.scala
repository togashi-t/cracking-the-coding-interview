package chapter1

object Question9 extends App {

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
