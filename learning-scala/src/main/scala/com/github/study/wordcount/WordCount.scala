package com.github.study.wordcount

class WordCount {

  //------------------------------------------------------
  // ワードカウント問題
  // https://gist.github.com/j5ik2o/7210762
  //------------------------------------------------------
  def countFruitsFromLines(lines: List[String]): Map[String, Int] =
    lines.flatMap(_.split(" ")).foldLeft(Map[String, Int]()) { (acc, x) =>
      acc ++ Map(x -> (acc.getOrElse(x, 0) + 1))
    }

  def countFruitsFromLines_(lines: List[String]): Map[String, Int] =
    lines.flatMap(_.split(" ")).groupBy(identity).mapValues(_.size)

}
