package com.github.study.rational

import org.scalatest._

class RationalSpec extends FunSpec with Matchers {

  describe("Rational") {

    describe("+") {

      it("should returns new rational") {
        val x = Rational(1, 3)
        val y = Rational(1, 3)
        x + y should be(Rational(2, 3))
      }

      it("should returns new rational even if have different denominators") {
        val x = Rational(1, 15)
        val y = Rational(1, 12)
        x + y should be(Rational(9, 60)) // ?!
      }

    }

    describe("compare") {

      it("should compare rational") {
        val x = Rational(1, 3)
        val y = Rational(1, 2)
        println(x, y)
        println(x < y)
        println(x > y)
        println(x <= y)
        println(x >= y)
      }

    }

  }

}
