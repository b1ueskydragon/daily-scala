package com.github.study.wordcount

import org.scalatest._

class WordCountSpec extends FunSpec with Matchers {

  describe("A WordCount") {
    it("count each fruit") {
      val lines = List("apple banana", "orange apple mango", "kiwi papaya orange", "mango orange muscat apple")
      val target = new WordCount

      val ans = target.countFruitsFromLines(lines)
      val ans_ = target.countFruitsFromLines_(lines)

      ans should be(Map("banana" -> 1, "muscat" -> 1, "orange" -> 3, "mango" -> 2, "apple" -> 3, "kiwi" -> 1, "papaya" -> 1))

      assert(ans_ === ans)

    }

  }

}
