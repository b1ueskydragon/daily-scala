package com.github.study.rational

// 分数
case class Rational(n: Int, d: Int) {
  require(d != 0)

  private def gcd(x: Int, y: Int): Int = if (y == 0) Math.abs(x) else gcd(y, x % y)

  def +(that: Rational): Rational = {
    val a = n * that.d + that.n * d
    val b = d * that.d
    val r = gcd(a, b)

    Rational(a / r, b / r)
  }

  def <(that: Rational): Boolean = ???

  def >(that: Rational): Boolean = ???

  def <=(that: Rational): Boolean = ???

  def >=(that: Rational): Boolean = ???

}
