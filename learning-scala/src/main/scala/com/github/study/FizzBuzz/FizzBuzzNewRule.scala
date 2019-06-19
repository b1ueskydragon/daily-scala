package com.github.study.FizzBuzz

/** FizzBuzzルール変更
  * - その２
  *  - １行で書く
  * - その３
  *  - 2の倍数は除外する
  * - その４
  *  - １行ずつ表示をやめる
  *  - カンマ区切りで１行で出す
  * - その５
  *  - その1 バージョンのFizzBuzzの出力前のリストから、
  * Fizz/Buzz/FizzBuzz になっていない数値の足し算をして合計値を出す
  * - その６
  *  - 1 to 100のリストを作らない
  */
object FizzBuzzNewRule {

  def fizzBuzz02(n: Int): String =
    if (n % 15 == 0) "FizzBuzz" else if (n % 3 == 0) "Fizz" else if (n % 5 == 0) "Buzz" else s"$n"


  def fizzBuzz03(n: Int): String =
    if (n % 15 == 0) "FizzBuzz" else if (n % 3 == 0) "Fizz" else if (n % 5 == 0) "Buzz" else if (n % 2 == 0) "" else s"$n"

  def fizzBuzz05(xs: List[Int]): Int = xs.filterNot(_ % 15 == 0).filterNot(_ % 5 == 0).filterNot(_ % 3 == 0).sum

  def main(args: Array[String]): Unit = {

    val range = 1 to 20

    println("Rule 02")
    range.foreach { n => println(fizzBuzz02(n)) }

    println()

    println("Rule 03")
    range.foreach { n => val fb = fizzBuzz03(n); if (fb != "") println(s"$fb") }

    println()


    println("Rule 04")
    range.foreach { n => val fb = fizzBuzz03(n); if (n == range.last) print(s"$fb") else if (fb != "") print(s"$fb,") }

    println()
    println()

    println("Rule 05")
    println(fizzBuzz05(range.toList))

  }

}
