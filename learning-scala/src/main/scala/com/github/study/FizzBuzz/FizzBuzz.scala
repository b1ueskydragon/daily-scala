package com.github.study.FizzBuzz

object FizzBuzz {

  def fizzBuzz(x: Int): String = x match {
    case _ if x % 15 == 0 => "FizzBuzz"
    case _ if x % 3 == 0 => "Fizz"
    case _ if x % 5 == 0 => "Buzz"
    case _ => x.toString
  }

}
