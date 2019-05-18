package com.github.study.FizzBuzz

import org.scalatest.{FunSpec, Matchers}

/** Created by b1ueskydragon on 2019/05/18. */
class FizzBuzzSpec extends FunSpec with Matchers {

  def inRange(step: Int, end: Int)(f: Range => Unit) {
    f(Range(step, end, step))
  }


  def inRange(step: Int, end: Int, skip: Int)(f: Seq[Int] => Unit) {
    f(Range(step, end, step).filterNot(_ % skip == 0))
  }

  describe("fizzBuzz") {

    describe("should be Fizz when num is Multiple of 3") {

      inRange(step = 3, end = 100, skip = 5) {
        _.foreach(FizzBuzz.fizzBuzz(_) should be("Fizz"))
      }

    }

    describe("should be Buzz when num is Multiple of 5") {

      inRange(step = 5, end = 100, skip = 3) {
        _.foreach(FizzBuzz.fizzBuzz(_) should be("Buzz"))
      }

    }

    describe("should be FizzBuzz when num is an least common multiple of 3 and 5") {

      inRange(step = 15, end = 100) {
        _.foreach(FizzBuzz.fizzBuzz(_) should be("FizzBuzz"))
      }

    }

  }

}
