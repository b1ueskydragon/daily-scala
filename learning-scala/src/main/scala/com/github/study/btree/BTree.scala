package com.github.study.btree


sealed trait Node {
  def size: Int
}

case class Leaf(value: Int) extends Node {
  def size: Int = 1
}

case class Branch(left: Node, value: Int, right: Node) extends Node {
  def size: Int = left.size + 1 + right.size
}

case class BTree(node: Node) {
  def size: Int = node.size

  def max: Int = ???

  def min: Int = ???

  def avg: Int = ???

  def sum: Int = ???
}

object BTree {
  def apply(list: List[Int]): BTree = ???
}

