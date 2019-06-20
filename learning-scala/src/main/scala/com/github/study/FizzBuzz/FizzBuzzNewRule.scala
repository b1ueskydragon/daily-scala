package com.github.study.FizzBuzz

//case class FizzBuzzElement(
//  ret: String // Either[String, Int]
//) {
//  def toNumber: Option[Int] = ???
//}

/** Separate Collection(create collection) and Output(println etc) */
object FizzBuzzNewRule {

  def fizzBuzz(n: Int): String = if (n % 15 == 0) "FizzBuzz" else if (n % 5 == 0) "Buzz" else if (n % 3 == 0) "Fizz" else s"$n"

  def fizzBuzz03(ns: Range): Seq[String] = ns.filterNot(_ % 2 == 0).map(fizzBuzz)

  def fizzBuzz04(ns: Range): String = ns.map(fizzBuzz).mkString(",")

  def fizzBuzz05(ns: Range): Int = ns.filterNot(x => x % 3 == 0 || x % 5 == 0).sum

  def fizzBuzz05_(n: Int): Int = (1 + n) * n / 2 - (1 + n / 5) * 5 * n / 5 / 2 - (1 + n / 3) * 3 * (n / 3) / 2 + (1 + n / 15) * 15 * (n / 15) / 2

  def fizzBuzz05__(ns: Range): Int = ns.map(n => fizzBuzz(n)).filter(_.forall(Character.isDigit)).map(_.toInt).sum

  def fizzBuzz06: Stream[String] = {

    def three: Stream[Option[String]] = Stream(None, None, Some("Fizz")) #::: three

    def five: Stream[Option[String]] = Stream(None, None, None, None, Some("Buzz")) #::: five

    three.zip(five).zip(Stream.from(1)).map {
      case ((None, None), k) => k.toString
      case ((f, b), _) => s"${f.getOrElse("")}${b.getOrElse("")}"
    }

  }

  def main(args: Array[String]): Unit = {

    val range = 1 to 20

    rule("02")(range.map(fizzBuzz).out())

    rule("03")(fizzBuzz03(range).out())

    rule("04")(fizzBuzz04(range).out())

    rule("05")(fizzBuzz05(range).out())

    rule("05_")(fizzBuzz05_(range.last).out()) // TODO fix bug if range is not 20

    rule("05__")(fizzBuzz05__(range).out())

    rule("06")(fizzBuzz06.take(range.last).out())

  }


  implicit class RichOut[T](x: T) {
    def out() {
      println(x)
    }
  }

  implicit class RichOutSeq[T](xs: Seq[T]) {
    def out() {
      xs.foreach(println)
    }
  }

  private def rule(num: String)(f: => Unit) {
    println(s"[ RULE $num ]")
    f
    println()
  }

}
