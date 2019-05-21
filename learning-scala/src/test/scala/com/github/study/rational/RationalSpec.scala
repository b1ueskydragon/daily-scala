package com.github.study.rational

import org.scalatest._

class RationalSpec extends FunSpec with Matchers {

  describe("Rational") {

    describe("should have non-zero denominator") {
      intercept[IllegalArgumentException] {
        Rational(3, 0)
      }
    }

    describe("+") {

      it("should returns new rational") {
        val x = Rational(1, 3)
        val y = Rational(1, 3)
        x + y should be(Rational(2, 3))
      }

      it("should returns new normalized rational even if have different denominators") {
        val x = Rational(1, 15)
        val y = Rational(1, 12)
        x + y should be(Rational(3, 20))
      }

    }

    describe("compare") {

      it("should compare rational") {
        val x = Rational(1, 3)
        val y = Rational(1, 2)
        println(x, y)
        x < y should be(true)
        x > y should be(false)
        x <= y should be(true)
        x >= y should be(false)
      }

    }

  }

}
