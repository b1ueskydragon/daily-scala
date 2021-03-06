package com.github.study.FizzBuzz

object FizzBuzz {

  def fizzBuzz(x: Int): String = x match {
    case _ if x % 15 == 0 => "FizzBuzz"
    case _ if x % 3 == 0 => "Fizz"
    case _ if x % 5 == 0 => "Buzz"
    case _ => x.toString
  }

  def fizzBuzzFoldr(xs: List[Int]): List[String] = xs.foldRight(List.empty[String]) { (x, acc) =>
    (if (x % 15 == 0) "FizzBuzz" else if (x % 3 == 0) "Fizz" else if (x % 5 == 0) "Buzz" else s"$x") :: acc
  }

  def fizzBuzzListzz(xs: List[Int]): List[String] =
    xs.zip(List.fill(xs.length)(List(
      None, None, Some("Fizz"), None, Some("Buzz"),
      Some("Fizz"), None, None, Some("Fizz"), Some("Buzz"),
      None, Some("Fizz"), None, None, Some("FizzBuzz")
    )).flatten).map(z => z._2.getOrElse(s"${z._1}"))

  def fizzBuzzIte(sortedInc: Range): Iterable[String] =
    sortedInc.map {
      x => (x % 3, x % 5, x)
    }.map {
      case (0, 0, _) => "FizzBuzz"
      case (0, _, _) => "Fizz"
      case (_, 0, _) => "Buzz"
      case (_, _, x) => x.toString
    }

  def fizzBuzzStreamAny(from: Int): Stream[Any] = {

    def three(x: Int): Stream[Any] = Stream(x, x + 1, "Fizz") #::: three(x + 3)

    def five(x: Int): Stream[Any] = Stream(x, x + 1, x + 2, x + 3, "Buzz") #::: five(x + 5)

    five(from).zip(three(from)).map {
      case (b: String, f: String) => f + b
      case (_, f: String) => f
      case (b: String, _) => b
      case x => x._1
    }

  }

  def fizzBuzzStreamString(from: Int): Stream[String] = {

    def three(x: Int): Stream[String] = Stream(s"$x", s"${x + 1}", "Fizz") #::: three(x + 3)

    def five(x: Int): Stream[String] = Stream(s"$x", s"${x + 1}", s"${x + 2}", s"${x + 3}", "Buzz") #::: five(x + 5)

    five(from).zip(three(from)).map {
      case ("Buzz", "Fizz") => "FizzBuzz"
      case (_, "Fizz") => "Fizz"
      case ("Buzz", _) => "Buzz"
      case x => x._1
    }

  }

  def fizzBuzzStreamTypeSafe(from: Int): Stream[String] = {

    // if make a recursive stream with `val`, make `lazy`
    // since to prevent `Forward reference extends over definition of value`

    lazy val three: Stream[Option[String]] = Stream(None, None, Some("Fizz")) #::: three

    lazy val five: Stream[Option[String]] = Stream(None, None, None, None, Some("Buzz")) #::: five

    three.zip(five).zip(Stream.from(from)).map {
      case ((None, None), k) => k.toString
      case ((f, b), _) => f.getOrElse("") + b.getOrElse("")
    }

  }

  def testFizzBuzz(f: (Int, String) => Unit) {
    val provider = Iterable((3, "Fizz"), (5, "Buzz"), (7, "7"), (15, "FizzBuzz"))
    provider foreach (e => f(e._1, e._2))
  }

}
