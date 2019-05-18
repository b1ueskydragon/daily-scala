package com.github.study.FizzBuzz

object FizzBuzz {

  def fizzBuzz(x: Int): String = x match {
    case _ if x % 15 == 0 => "FizzBuzz"
    case _ if x % 3 == 0 => "Fizz"
    case _ if x % 5 == 0 => "Buzz"
    case _ => x.toString
  }

  def fizzBuzz_(sortedInc: Range): Iterable[String] =
    sortedInc.map {
      x => (x % 3, x % 5, x)
    }.map {
      case (0, 0, _) => "FizzBuzz"
      case (0, _, _) => "Fizz"
      case (_, 0, _) => "Buzz"
      case (_, _, x) => x.toString
    }

  def main(args: Array[String]): Unit = {
    println(fizzBuzz_(1 to 20))
  }

}
