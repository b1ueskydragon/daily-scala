package com.github.study.FizzBuzz

case class FizzBuzzElement(ret: Either[String, Int]) {
  def toNumber: Option[Int] = ret.fold(_ => None, Some(_))
}

object FizzBuzzElement {
  def apply(n: Int): FizzBuzzElement = {
    val x = if (n % 15 == 0) Left("FizzBuzz") else if (n % 5 == 0) Left("Buzz") else if (n % 3 == 0) Left("Fizz") else Right(n)
    FizzBuzzElement(x)
  }
}

object FizzBuzzType {

  def main(args: Array[String]): Unit = {

    val range = 1 to 20

    rule("03")(range.withFilter(_ % 2 != 0).map(FizzBuzzElement(_)).map(_.ret.fold(s => s, i => s"$i")).out())

    rule("05")(range.map(FizzBuzzElement(_)).flatMap(_.toNumber).sum.out())

  }

  implicit class RichOutSeq[T](xs: Seq[T]) {
    def out() {
      xs.foreach(println)
    }
  }

  implicit class RichOut[T](x: T) {
    def out() {
      println(x)
    }
  }

  private def rule(num: String)(f: => Unit) {
    println(s"[ RULE $num ]")
    f
    println()
  }


}
