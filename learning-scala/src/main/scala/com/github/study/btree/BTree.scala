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

  def max: Int = node match { // most right
    case Leaf(v) => v
    case Branch(l, v, r) => Math.max(Math.max(BTree(l).max, v), BTree(r).max)
  }

  def min: Int = node match { // most left
    case l: Leaf => l.value
    case b: Branch => Math.min(Math.min(BTree(b.left).min, b.value), BTree(b.right).min)
  }

  def sum: Int = node match {
    case l@Leaf(_) => l.value
    case b@Branch(_, _, _) => BTree(b.left).sum + b.value + BTree(b.right).sum
  }

  def avg: Int = sum / size // middle value

}

object BTree {
  def apply(list: List[Int]): BTree = list match {
    case List(x) => BTree(Leaf(x))
    case l@List(_, _, _) => BTree(Branch(Leaf(l.min), l.sum / 3, Leaf(l.max)))
    case _ => ???
  }

}

