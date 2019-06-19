package com.github.study.FizzBuzz

object FizzBuzzNewRule {

  def fizzBuzz03(n: Int): String =
    if (n % 15 == 0) "FizzBuzz" else if (n % 3 == 0) "Fizz" else if (n % 5 == 0) "Buzz" else if (n % 2 == 0) "" else s"$n"

  def fizzBuzz05(xs: List[Int]): Int = xs.filterNot(x => x % 5 == 0 || x % 3 == 0).sum

  def fizzBuzz06(from: Int): Stream[String] = {

    def three: Stream[Option[String]] = Stream(None, None, Some("Fizz")) #::: three

    def five: Stream[Option[String]] = Stream(None, None, None, None, Some("Buzz")) #::: five

    three.zip(five).zip(Stream.from(1)).map {
      case ((None, None), k) => k.toString
      case ((f, b), _) => s"${f.getOrElse("")}${b.getOrElse("")}"
    }

  }

  def main(args: Array[String]): Unit = {

    val range = 1 to 20

    rule("02") {
      range.foreach { i => println(if (i % 15 == 0) "FizzBuzz" else if (i % 5 == 0) "Buzz" else if (i % 3 == 0) "Fizz" else i) }
    }

    rule("03") {
      range.foreach { n => val fb = fizzBuzz03(n); if (fb != "") println(s"$fb") }
    }

    rule("04") {
      range.foreach { n => val fb = fizzBuzz03(n); if (n == range.last) println(s"$fb") else if (fb != "") print(s"$fb,") }
    }

    rule("05") {
      println(fizzBuzz05(range.toList))
    }

    rule("06") {
      fizzBuzz06(range.head).take(range.last).foreach(println)
    }

  }

  private def rule(num: String)(f: => Unit) {
    println(s"[ RULE $num ]")
    f
    println()
  }

}
