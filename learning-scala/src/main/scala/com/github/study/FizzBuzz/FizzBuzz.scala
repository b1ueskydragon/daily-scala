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

  def fizzBuzzStream(from: Int) = {

    // if make a recursive stream with `val`, make `lazy`
    // since to prevent `Forward reference extends over definition of value`

    def three(x: Int): Stream[Any] = Stream(x, x + 1, "Fizz") #::: three(x + 3)

    def five(x: Int): Stream[Any] = Stream(x, x + 1, x + 2, x + 3, "Buzz") #::: five(x + 5)

    five(from).zip(three(from)).map {
      case (b: String, f: String) => f + b
      case (_, f: String) => f
      case (b: String, _) => b
      case x => x._1
    }

  }

  def fizzBuzzTypeSafe(from: Int) = {

    def three(x: Int): Stream[String] = Stream(s"$x", s"${x + 1}", "Fizz") #::: three(x + 3)

    def five(x: Int): Stream[String] = Stream(s"$x", s"${x + 1}", s"${x + 2}", s"${x + 3}", "Buzz") #::: five(x + 5)

    five(from).zip(three(from)).map {
      case ("Buzz", "Fizz") => "FizzBuzz"
      case (_, "Fizz") => "Fizz"
      case ("Buzz", _) => "Buzz"
      case x => x._1
    }

  }

  def main(args: Array[String]): Unit = {

    println(fizzBuzz_(1 to 20).toList)
    println(fizzBuzzStream(1).take(20).toList)
    println(fizzBuzzTypeSafe(1).take(20).toList)


  }

}
