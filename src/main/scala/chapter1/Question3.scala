package chapter1

object Question3 extends App {


  // 1-3。空白文字を指定文字で置換。ただし、strについては指定された長さ以降は切り捨てる。
  def replaceAllSpace(str: String, trueLength: Int): String = {
    str.take(trueLength).replaceAll(" ", "%20")
  }

  val str = "Mr John Smith "
  val res = replaceAllSpace(str, 13)
  println(res == "Mr%20John%20Smith")
}
