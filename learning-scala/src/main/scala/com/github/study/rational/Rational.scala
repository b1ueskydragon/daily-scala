package com.github.study.rational

// 分数
case class Rational(n: Int, d: Int) {
  require(d != 0)

  private def lcm(x: Int, y: Int): Int = (x * y) / gcd(x, y)

  private def gcd(x: Int, y: Int): Int = if (y == 0) Math.abs(x) else gcd(y, x % y)

  def +(that: Rational): Rational = (d, that.d) match {
    case (x, y) if x == y => Rational(n + that.n, d)
    case (x, y) =>
      val lcd = lcm(x, y)
      Rational(n * (lcd / d) + that.n * (lcd / that.d), lcd)
  }

  def <(that: Rational): Boolean = ???

  def >(that: Rational): Boolean = ???

  def <=(that: Rational): Boolean = ???

  def >=(that: Rational): Boolean = ???

}
